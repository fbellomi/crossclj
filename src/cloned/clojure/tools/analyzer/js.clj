;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cloned.clojure.tools.analyzer.js
  "Analyzer for clojurescript code, extends tools.analyzer with JS specific passes/forms"
  (:refer-clojure :exclude [macroexpand-1 var? *ns* ns-resolve])
  (:require [cloned.clojure.tools.analyzer
             :as ana
             :refer [analyze analyze-in-env]
             :rename {analyze -analyze}]
            [cloned.clojure.tools.analyzer
             [utils :refer [resolve-ns ctx -source-info dissoc-env const-val mmerge update-vals] :as u]
             [ast :refer [prewalk postwalk]]
             [env :as env :refer [*env*]]
             [passes :refer [schedule]]]
            [cloned.clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [cleanup :refer [cleanup]]
             [elide-meta :refer [elide-meta elides]]
             [warn-earmuff :refer [warn-earmuff]]
             [add-binding-atom :refer [add-binding-atom]]
             [uniquify :refer [uniquify-locals]]]
            [cloned.clojure.tools.analyzer.passes.js
             [annotate-tag :refer [annotate-tag]]
             [infer-tag :refer [infer-tag]]
             [validate :refer [validate]]
             [collect-keywords :refer [collect-keywords]]
             [analyze-host-expr :refer [analyze-host-expr]]]
            [cloned.clojure.tools.analyzer.js.utils
             :refer [desugar-ns-specs validate-ns-specs ns-resource ns->relpath res-path]]
            [cljs
             [env :as cljs.env]
             [analyzer :as cljs.ana]
             [tagged-literals :as tags]
             [js-deps :as deps]]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [cloned.clojure.tools.reader :as reader]
            [cloned.clojure.tools.reader.reader-types :as readers])
  (:import cljs.tagged_literals.JSValue)
  (:alias c.c clojure.core))

(def specials
  "Set of the special forms for clojurescript"
  (into ana/specials '#{ns deftype* defrecord* js* case*}))

(defmulti parse
  "Extension to tools.analyzer/-parse for CLJS special forms"
  (fn [[op & rest] env] op))

(defmethod parse :default
  [form env]
  (ana/-parse form env))

(def ^:dynamic *ns* 'cljs.user)

(defonce core-env (atom {}))

(defn global-env []
  (atom (merge (and cljs.env/*compiler* @cljs.env/*compiler*)
               {:namespaces          (merge '{goog {:mappings {}, :js-namespace true, :ns goog}
                                              Math {:mappings {}, :js-namespace true, :ns Math}}
                                            @core-env)
                :js-dependency-index (deps/js-dependency-index {})})))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/statement
   :locals     {}
   :ns         *ns*})

(defn fix-ns [ns]
  (case ns
    ("clojure.core" "cloned.clojure.tools.analyzer.js.cljs.core")
    "cljs.core"
    ns))

(defn fix-ns-macro [ns]
  (let [ns (fix-ns ns)]
    (if (= "cljs.core" ns)
      "cloned.clojure.tools.analyzer.js.cljs.core"
      ns)))

(defn fix-symbol [sym]
  (symbol (fix-ns (namespace sym)) (name sym)))

(defn ns-resolve [ns sym]
  (let [ns (if (string? ns)
             (symbol ns)
             ns)
        sym (if (string? sym)
              (symbol sym)
              sym)]
    (and (find-ns ns)
         (c.c/ns-resolve ns sym))))

(defn maybe-macro [sym {:keys [ns]}]
  (let [var (if-let [sym-ns (fix-ns-macro (namespace sym))]
              (if-let [full-ns (get-in (env/deref-env)
                                       [:namespaces ns :macro-aliases (symbol sym-ns)])]
                (ns-resolve full-ns (name sym))
                (ns-resolve sym-ns (name sym)))
              (get-in (env/deref-env) [:namespaces ns :macro-mappings sym]))]
    (when (:macro (meta var))
      var)))

(defn resolve-sym [sym env]
  (or (u/resolve-sym (fix-symbol sym) env)
      (get-in env [:locals sym])))

(defn dotted-symbol? [form env]
  (let [n (name form)
        ns (namespace form)
        idx (.indexOf n ".")
        sym (and (pos? idx)
                 (symbol ns (.substring n 0 idx)))]
    (and (not= idx -1)
         (not (resolve-sym form env))
         (not= sym form)
         (resolve-sym sym env))))

(defn desugar-symbol [form env]
  (let [ns (fix-ns (namespace form))
        n (name form)
        form (symbol ns n)]
    (if (dotted-symbol? form env)
      (let [idx (.indexOf n ".")
            sym (symbol ns (.substring n 0 idx))]
        (list '. sym (symbol (str "-" (.substring n (inc idx) (count n))))))

      form)))

(defn desugar-host-expr [form env]
  (if (symbol? (first form))
    (let [[op & expr] form
          opname (name op)
          opns   (namespace op)]
      (cond

       ;; (.foo bar ..) -> (. bar foo ..)
       (= (first opname) \.)
       (let [[target & args] expr
             args (list* (symbol (subs opname 1)) args)]
         (list '. target (if (= 1 (count args))
                           (first args) args)))

       ;; (foo. ..) -> (new foo ..)
       (= (last opname) \.)
       (let [op-s (str op)]
         (list* 'new (symbol (subs op-s 0 (dec (count op-s)))) expr))

       ;; (var.foo ..) -> (. var foo ..)
       (dotted-symbol? op env)
       (let [idx (.indexOf opname ".")
             sym (symbol opns (.substring opname 0 idx))]
         (list '. sym (list* (symbol (.substring opname (inc idx) (count opname))) expr)))

       :else (list* (fix-symbol op) expr)))
    form))

(defn macroexpand-1 [form env]
  "If form represents a macro form returns its expansion, else returns form."
  (env/ensure (global-env)
    (if (seq? form)
      (let [op (first form)]
        (if (or (not (symbol? op))
                (specials op))
          form
          (if-let [clj-macro (and (not (-> env :locals (get op)))
                                  (maybe-macro op env))]
            (with-bindings (merge {#'c.c/*ns* (create-ns *ns*)}
                                  (when-not (thread-bound? #'*ns*)
                                    {#'*ns* *ns*}))
              (let [ret (apply clj-macro form env (rest form))] ; (m &form &env & args)
                (if (and (seq? ret)
                         (= 'js* (first ret)))
                  (vary-meta ret merge
                             (when (-> clj-macro meta :cljs.analyzer/numeric)
                               {:tag 'number}))
                  ret)))
            (with-meta (desugar-host-expr form env) (meta form)))))
      (with-meta (desugar-symbol form env) (meta form)))))

(defn create-var
  "Creates a var map for sym and returns it."
  [sym {:keys [ns]}]
  (with-meta {:op   :var
              :name sym
              :ns   ns}
    (meta sym)))

(defn var? [x]
  (= :var (:op x)))

;; can it be :literal ?
(defn analyze-js-value
  [form env]
  (let [val (.val ^JSValue form)
        items-env (ctx env :expr)]
    (if (map? val)
      ;; keys should always be symbols/kewords, do we really need to analyze them?
      {:op       :js-object
       :env      env
       :keys     (mapv (analyze-in-env items-env) (keys val))
       :vals     (mapv (analyze-in-env items-env) (vals val))
       :form     form
       :children [:keys :vals]}
      {:op       :js-array
       :env      env
       :items    (mapv (analyze-in-env items-env) val)
       :form     form
       :children [:items]})))

(defn analyze-form
  [form env]
  (if (instance? JSValue form)
    (analyze-js-value form env)
    (ana/-analyze-form form env)))

(defn parse-type
  [op [_ name fields pmasks body :as form] {:keys [ns] :as env}]
  (let [fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :mutable (:mutable (meta name))
                             :local   :field
                             :op      :binding})
                          fields)
        protocols (-> name meta :protocols)

        _ (swap! *env* assoc-in [:namespaces ns :mappings name]
                 {:op        :var
                  :type      true
                  :name      name
                  :ns        ns
                  :fields    fields
                  :protocols protocols})

        body-expr (-analyze body (assoc env
                                   :locals (zipmap fields (map dissoc-env fields-expr))))]

    {:op        op
     :env       env
     :form      form
     :name      name
     :fields    fields-expr
     :body      body-expr
     :pmasks    pmasks
     :protocols protocols
     :children  [:fields :body]}))

(defmethod parse 'deftype*
  [form env]
  (parse-type :deftype form env))

(defmethod parse 'defrecord*
  [form env]
  (parse-type :defrecord form env))

;; no ~{foo} support since cljs itself doesn't use it anywhere
(defmethod parse 'js*
  [[_ jsform & args :as form] env]
  (when-not (string? jsform)
    (throw (ex-info "Invalid js* form"
                    (merge {:form form}
                           (-source-info form env)))))
  (let [segs  (loop [segs [] ^String s jsform]
                (let [idx (.indexOf s "~{")]
                  (if (= -1 idx)
                    (conj segs s)
                    (recur (conj segs (subs s 0 idx))
                           (subs s (inc (.indexOf s "}" idx)))))))
        exprs (mapv (analyze-in-env (ctx env :ctx/expr)) args)]
    (merge
     {:op       :js
      :env      env
      :form     form
      :segs     segs}
     (when args
       {:args     exprs
        :children [:args]}))))

(defmethod parse 'case*
  [[_ test tests thens default :as form] env]
  (assert (symbol? test) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (ctx env :expr)
        test-expr (-analyze test expr-env)
        nodes (mapv (fn [tests then]
                      {:op       :case-node
                       ;; no :form, this is a synthetic grouping node
                       :env      env
                       :tests    (mapv (fn [test]
                                         {:op       :case-test
                                          :form     test
                                          :env      expr-env
                                          :test     (-analyze test expr-env)
                                          :children [:test]})
                                       tests)
                       :then     {:op       :case-then
                                  :form     test
                                  :env      env
                                  :then     (-analyze then env)
                                  :children [:then]}
                       :children [:tests :then]})
                    tests thens)
        default-expr (-analyze default env)]
    (assert (every? (fn [t] (and (= :const (-> t :test :op))
                           ((some-fn number? string?) (:form t))))
               (mapcat :tests nodes))
            "case* tests must be numbers or strings")
    {:op       :case
     :form     form
     :env      env
     :test     (assoc test-expr :case-test true)
     :nodes    nodes
     :default  default-expr
     :children [:test :nodes :default]}))

(def ^:private ^:dynamic *deps-map* {:path [] :deps #{}})
(declare analyze-ns)

(defn ensure-loaded [ns {:keys [refer]}]
  (assert (not (contains? (:deps *deps-map*) ns))
          (str "Circular dependency detected :" (conj (:path *deps-map*) ns)))
  (binding [*deps-map* (-> *deps-map*
                         (update-in [:path] conj ns)
                         (update-in [:deps] conj ns))]
    (let [namespaces (-> (env/deref-env) :namespaces)]
      (or (and (get namespaces ns)
               (not (get-in namespaces [ns :js-namespace])))
          (and (get-in (env/deref-env) [:js-dependency-index (name ns)])
               (swap! env/*env* update-in [:namespaces ns] merge
                      {:ns           ns
                       :js-namespace true})
               (swap! env/*env* update-in [:namespaces ns :mappings] merge
                      (reduce (fn [m k] (assoc m k {:op   :js-var
                                                   :name k
                                                   :ns   ns}))
                              {} refer)))
          (analyze-ns ns)))))

(defn core-macros []
  (reduce-kv (fn [m k v]
               (if (:macro (meta v))
                 (assoc m k v)
                 m))
             {} (ns-interns 'cloned.clojure.tools.analyzer.js.cljs.core)))

(defn populate-env
  [{:keys [import require require-macros refer-clojure]} ns-name env]
  (let [imports (reduce-kv (fn [m prefix suffixes]
                             (merge m (into {} (mapv (fn [s] [s {:op   :js-var
                                                                :ns   prefix
                                                                :name s}]) suffixes)))) {} import)
        require-aliases (reduce (fn [m [ns {:keys [as]}]]
                                  (if as
                                    (assoc m as ns)
                                    m)) {} require)
        require-mappings (reduce (fn [m [ns {:keys [refer] :as spec}]]
                                   (ensure-loaded ns spec)
                                   (reduce #(assoc %1 %2 (get-in (env/deref-env)
                                                                 [:namespaces ns :mappings %2])) m refer))
                                 {} require)
        core-mappings (apply dissoc (get-in (env/deref-env) [:namespaces 'cljs.core :mappings]) (:exclude refer-clojure))
        macro-aliases (reduce (fn [m [ns {:keys [as]}]]
                                (if as
                                  (assoc m as ns)
                                  m)) {} require-macros)
        core-macro-mappings (apply dissoc (core-macros) (:exclude refer-clojure))
        macro-mappings (reduce (fn [m [ns {:keys [refer]}]]
                                 (c.c/require ns)
                                 (reduce #(let [m (ns-resolve ns (name %2))]
                                            (if (:macro (meta m))
                                              (assoc %1 %2 m)
                                              %1)) m refer))
                               {} require-macros)]

    (swap! *env* assoc-in [:namespaces ns-name]
           {:ns             ns-name
            :mappings       (merge core-mappings require-mappings imports)
            :aliases        require-aliases
            :macro-mappings (merge core-macro-mappings macro-mappings)
            :macro-aliases  macro-aliases})))

(defmethod parse 'ns
  [[_ name & args :as form] env]
  (when-not (symbol? name)
    (throw (ex-info (str "Namespaces must be named by a symbol, had: "
                         (.getName ^Class (class name)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [[docstring & args] (if (string? (first args))
                             args
                             (cons nil args))
        [metadata & args]  (if (map? (first args))
                             args
                             (cons {} args))
        name (vary-meta name merge metadata)
        ns-opts (doto (desugar-ns-specs args form env)
                  (validate-ns-specs form env)
                  (populate-env name env))]
    (set! *ns* name)
    (merge
     {:op      :ns
      :env     env
      :form    form
      :name    name
      :depends (set (keys (:require ns-opts)))}
     (when docstring
       {:doc docstring})
     (when metadata
       {:meta metadata}))))

(defmethod parse 'def
  [[_ sym & rest :as form] env]
  (let [ks #{:ns :name :doc :arglists :file :line :column}
        meta (meta sym)
        m (merge {}
                 (update-vals (select-keys meta ks) (fn [x] (list 'quote x)))
                 (when (:test meta)
                   {:test `(.-cljs$lang$test ~sym)}))]
    (ana/-parse (with-meta `(def ~(with-meta sym m) ~@rest) (meta form)) env)))

(def default-passes
  "Set of passes that will be run by default on the AST by #'run-passes"
  #{#'warn-earmuff

    #'uniquify-locals

    #'source-info
    #'elide-meta

    #'collect-keywords

    #'validate
    #'infer-tag})

(def scheduled-default-passes
  (schedule default-passes))

(defn ^:dynamic run-passes
  "Function that will be invoked on the AST tree immediately after it has been constructed,
   by default set-ups and runs the default passes declared in #'default-passes"
  [ast]
  (scheduled-default-passes ast))

(defn analyze
  "Returns an AST for the form.

   Binds tools.analyzer/{macroexpand-1,create-var,parse} to
   tools.analyzer.js/{macroexpand-1,create-var,parse} and analyzes the form.

   If provided, opts should be a map of options to analyze, currently the only valid
   options are :bindings and :passes-opts.
   If provided, :bindings should be a map of Var->value pairs that will be merged into the
   default bindings for tools.analyzer, useful to provide custom extension points.
   If provided, :passes-opts should be a map of pass-name-kw->pass-config-map pairs that
   can be used to configure the behaviour of each pass.

   E.g.
   (analyze form env {:bindings  {#'ana/macroexpand-1 my-mexpand-1}})

   Calls `run-passes` on the AST."
  ([form] (analyze form (empty-env) {}))
  ([form env] (analyze form env {}))
  ([form env opts]
     (with-bindings (merge {#'ana/macroexpand-1 macroexpand-1
                            #'ana/create-var    create-var
                            #'ana/parse         parse
                            #'ana/var?          var?
                            #'ana/analyze-form  analyze-form
                            #'elides            (-> elides
                                                  (update-in [:all] into #{:line :column :end-line :end-column :file :source})
                                                  (assoc-in [:fn] #{:cljs.analyzer/type :cljs.analyzer/protocol-impl :cljs.analyzer/protocol-inline}))}
                           (when-not (thread-bound? #'*ns*)
                             {#'*ns* *ns*})
                           (:bindings opts))
       (env/ensure (global-env)
         (swap! env/*env* mmerge {:passes-opts (:passes-opts opts)})
         (run-passes (-analyze form env))))))

(defn analyze-ns
  "Analyzes a whole namespace, returns a vector of the ASTs for all the
   top-level ASTs of that namespace."
  [ns]
  (env/ensure (global-env)
    (let [res (ns-resource ns)]
      (assert res (str "Can't find " ns " in classpath"))
      (let [filename (ns->relpath ns)
            path (res-path res)]
        (when-not (get-in *env* [::analyzed-cljs path])
          (binding [*ns*   *ns*
                    *file* filename]
            (with-open [rdr (io/reader res)]
              (let [pbr (readers/indexing-push-back-reader
                         (java.io.PushbackReader. rdr) 1 filename)
                    eof (Object.)
                    env (empty-env)]
                (loop []
                  (let [form (binding [c.c/*ns* (create-ns *ns*)
                                       reader/*data-readers* tags/*cljs-data-readers*
                                       reader/*alias-map* (apply merge {}
                                                                 (-> (env/deref-env) :namespaces (get *ns*)
                                                                    (select-keys #{:aliases :macro-aliases})
                                                                    vals))]
                               (reader/read pbr nil eof))]
                    (when-not (identical? form eof)
                      (swap! *env* update-in [::analyzed-cljs path]
                             (fnil conj [])
                             (analyze form (assoc env :ns *ns*)))
                      (recur))))))))
        (get-in @*env* [::analyzed-cljs path])))))

(defn backup-env
  "Caches the current namespaces state in a resource file, can be restored with
   (restore-env)"
  []
  (env/ensure (global-env)
    (with-redefs [clojure.core/pr-on (fn [x w] (if (clojure.core/var? x)
                                                (print-dup x w)
                                                (print-method x w))
                                       nil)]
      (binding [*print-level* nil
                *print-length* nil
                *print-meta* true]
        (let [s (pr-str (:namespaces (env/deref-env)))]
          (spit (io/resource "tools.analyzer.js/cached-env.res") s))))))

(defn restore-env
  "Uses a cached env to populate the default namespace map"
  []
  (reset! core-env
          (reader/read-string (slurp (io/resource "tools.analyzer.js/cached-env.res")))))

(defn setup-rt!
  "Setups the basic runtime, loading cljs.core and initializing cljs.user"
  []
  (require 'cloned.clojure.tools.analyzer.js.cljs.core)
  (when-not (or (seq @core-env)
                #_(seq (restore-env)))
    (env/with-env (global-env)
      (analyze-ns 'cljs.core)
      (analyze '(ns cljs.user))
      (reset! core-env (select-keys (:namespaces (env/deref-env)) '[cljs.core cljs.user])))))

(defn cljs-env->env
  "Converts the namespace map of the current cljs environment in a tools.analyzer.js
   namespace map and returns it."
  []
  (env/ensure (global-env)
    (reduce (fn [m {:keys [name excludes uses requires use-macros require-macros imports defs]}]
              (let [imports (reduce-kv (fn [m k v]
                                         (assoc m k (let [s (s/split (c.c/name v) #"\.")]
                                                      {:op   :js-var
                                                       :ns   (symbol (s/join "." (butlast s)))
                                                       :name (symbol (last s))}))) {} imports)
                    parse-requires (fn [r] (reduce-kv (fn [m k v] (if (not= k v)
                                                                  (assoc m k v)
                                                                  m)) {} r))
                    core-mappings (apply dissoc (get-in (env/deref-env) [:namespaces 'cljs.core :mappings]) excludes)
                    core-macro-mappings (apply dissoc (core-macros) excludes)
                    js-namespaces (reduce (fn [m ns] (assoc m ns {:ns ns :js-namespace true})) {} (set (vals requires)))

                    mappings (reduce-kv (fn [m k v] (assoc m k {:op   (if (js-namespaces v) :js-var :var)
                                                               :name k
                                                               :ns   v})) {} uses)
                    macro-mappings (reduce-kv (fn [m k v]
                                                (let [macro (ns-resolve v k)]
                                                  (if (:macro (meta macro))
                                                    (assoc m k macro)
                                                    m))) {} uses)
                    defs (reduce-kv (fn [m k v]
                                      (assoc m k {:op   :var
                                                  :name (vary-meta k merge (select-keys v #{:protocol-impl}))
                                                  :ns   name})) {} defs)]
                (merge m js-namespaces
                       {name {:ns             name
                              :mappings       (merge imports core-mappings mappings defs)
                              :macro-mappings (merge core-macro-mappings macro-mappings)
                              :aliases        (parse-requires requires)
                              :macro-aliases  (parse-requires require-macros)}})))
            {} (vals @cljs.ana/namespaces))))

(setup-rt!)
