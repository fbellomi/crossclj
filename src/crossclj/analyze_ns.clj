(ns crossclj.analyze-ns
  (:refer-clojure :exclude [macroexpand-1])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io File PrintWriter FileWriter)
           (java.net URL))
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cloned.clojure.tools.reader :as tr]

            [cloned.clojure.tools.analyzer
             [passes :refer [schedule]]]

            [cloned.clojure.tools.analyzer.jvm :as ana.jvm]
            [cloned.clojure.tools.analyzer.ast :refer [postwalk prewalk cycling]]
            [cloned.clojure.tools.analyzer :as ana :refer [analyze] :rename {analyze -analyze}]
            [cloned.clojure.tools.analyzer.env :as env]

            [cloned.clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [cleanup :refer [cleanup]]
             [elide-meta :refer [elide-meta elides]]
             [collect-closed-overs :refer [collect-closed-overs]]
             [uniquify :refer [uniquify-locals]]]

            [cloned.clojure.tools.analyzer.passes.jvm
             [box :refer [box]]
             [constant-lifter :refer [constant-lift]]
             [analyze-host-expr :refer [analyze-host-expr]]
             [classify-invoke :refer [classify-invoke]]
             [validate :refer [validate]]
             [infer-tag :refer [infer-tag]]
             [validate-loop-locals :refer [validate-loop-locals]]
             [warn-on-reflection :refer [warn-on-reflection]]
             [emit-form :refer [emit-form]]]

            [cljs.analyzer]
            [cljs.compiler]
            [cljs.env :as jsenv]

             #_[cloned.clojure.tools.analyzer.js :as jsa]
             #_[cloned.clojure.tools.analyzer.js.utils :as jsu]
            [clojure.string :as str]))

(def ex-log "exceptions.txt")

(defn log-exception [e [c art]]
  (let [w (PrintWriter. (FileWriter. ex-log true) true)]
    (.write w (str "---\n" [c art] \newline))
    (let [text (str (class e))
          text (if (> 1000 (count text)) text (subs text 0 1000))]
      (.write w (str text \newline)))
    (when (or (#{1 2 "global" "debug"} c)
              (instance? NullPointerException e))
      #_(.printStackTrace e w))
    (.close w)))

(defn munge-ns [ns-sym]
  (-> (name ns-sym)
      (string/replace "." "/")
      (string/replace "-" "_")
      (str ".clj")))

(defn uri-for-ns
  [ns-sym]
  (let [source-path (munge-ns ns-sym)
        uri (io/resource source-path)]
    (when-not uri
      (throw (Exception. (str "No file found for namespace " ns-sym))))
    uri))

(defn all-ns-names-set []
  (set (map str (all-ns))))

(def fake-value (fn [& args] (first args)))

(def fake-vars (atom '()))

(defn clean-fake-vars []
  (dorun (map (fn [x] (when (= (when-let [vv (find-var x)] (var-get vv)) fake-value)
                        (ns-unmap (symbol (namespace x)) (symbol (name x))))) @fake-vars))
  (reset! fake-vars '()))

(def default-passes0
  "Set of passes that will be run by default on the AST by #'run-passes"
  #{#'warn-on-reflection
    #'uniquify-locals

    #'source-info
    #'constant-lift

    #'box

    #'analyze-host-expr
    #'validate-loop-locals
    #'validate
    #'infer-tag

    #'classify-invoke})

(def scheduled-default-passes0
  (do #_(println (str/join \newline (map (fn [p] (str p " " (:pass-info (meta p)))) default-passes0))) (schedule default-passes0)))

(defn run-passes0
  "Function that will be invoked on the AST tree immediately after it has been constructed,
   by default set-ups and runs the default passes declared in #'default-passes"
  [ast]
  (scheduled-default-passes0 ast))

(defn unresolvable [ns var ast]
  (if ns
    (let [ns_ (create-ns ns)]
      (intern ns_ var fake-value)
      (swap! fake-vars conj (symbol (str ns_) (str var))))
    (do
      (intern (symbol (str *ns*)) (symbol (str var)) fake-value)
      (swap! fake-vars conj (symbol (str *ns*) (str var)))))
  ast)

(def opts {:passes-opts (merge {:validate/wrong-tag-handler (fn [_1 _2])
                                :validate/unresolvable-symbol-handler unresolvable}
                               ana.jvm/default-passes-opts)})
(defn analyze
  [form nsym]
  (binding [ana.jvm/run-passes run-passes0]
    (let [name (ns-name *ns*)
          name (if (= 'clojure.core name) nsym name)
          env (assoc (ana.jvm/empty-env) :ns name)]
      (ana.jvm/analyze form env
                       opts))))

(defn analyze-form [form source-nsym]
  (try
    (let [form-analysis (analyze form source-nsym)]
      {:exception nil :analysis form-analysis})
    (catch ThreadDeath e (throw e))
    (catch Throwable e
      {:exception e :analysis nil})))

(defn- analyze-file
  [source-path source-nsym & {:keys [reader]
                  :or {:reader (LineNumberingPushbackReader.
                               (io/reader (io/resource source-path)))}}]
  (let [eof (Object.)
        opts { :eof eof :features #{:clj} :read-cond :allow }
        ^LineNumberingPushbackReader
        pbrdr (if (instance? LineNumberingPushbackReader reader)
                reader
                (LineNumberingPushbackReader. reader))]

    (env/ensure (ana.jvm/global-env)
                (binding [*ns* *ns*
                          *warn-on-reflection* false
                          *print-meta* *print-meta*
                          *print-length* *print-length*
                          *print-level* *print-level*
                          *data-readers* *data-readers*
                          *default-data-reader-fn* *default-data-reader-fn*
                          *command-line-args* *command-line-args*
                          *unchecked-math* true
                          *file* (str source-path)]
                  (loop [forms []
                         asts []
                         unanalyzed-forms []]
                    (let [at-top-level? (empty? unanalyzed-forms)
                          [form unanalyzed-forms] (if at-top-level?
                                                    [(tr/read opts pbrdr) []]
                                                    [(first unanalyzed-forms)
                                                     (rest unanalyzed-forms)])
                          done? (and at-top-level?
                                     (identical? form eof))]
                      (ana.jvm/update-ns-map!)
                      #_(println "***\n" form)
                      (if done?
                        {:forms forms, :asts asts, :exception nil}
                        (let [{:keys [analysis exception]} (analyze-form form source-nsym)]
                          (if exception
                            (do (log-exception exception [1 source-path])
                                         #_(.printStackTrace exception)
                                (recur (conj forms form)
                                       asts
                                       unanalyzed-forms))
                            (do (try
                                  (eval (emit-form analysis))
                                  (catch ThreadDeath e (throw e))
                                  (catch Throwable e
                                         (log-exception e [2 source-path])
                                         #_(println "ERR2: " e)
                                         nil))
                                (recur (conj forms form)
                                       (conj asts (assoc analysis :original-form form))
                                       unanalyzed-forms)))))))))))


;; analyze-ns was copied from library jvm.tools.analyzer and then
;; modified

(defn analyze-ns
  [source-nsym source-path & {:keys [reader] :or {reader (io/reader (io/resource source-path))}}]
  (let [m (analyze-file source-path source-nsym :reader reader )]
    (clean-fake-vars)
    (assoc (dissoc m :forms :asts)
      :analyze-results {:source (slurp (io/resource source-path))
                        :namespace source-nsym
                        :forms (:forms m)
                        :asts (:asts m)})))

; --- cljs

(defn analyze-ns-cljs [ns f]
  (let [res (cond
             (instance? File f) f
             (instance? URL f) f
             (re-find #"^file://" f) (URL. f)
             :else (io/resource f))]
    (cljs.env/ensure
      (let [path f]
        (when-not false #_(get-in @jsenv/*compiler* [:cljs.analyzer/analyzed-cljs path])
          (binding [cljs.analyzer/*cljs-ns* 'cljs.user
                    cljs.analyzer/*cljs-file* path
                    cljs.analyzer/*cljs-warnings* {}
                    tr/*alias-map* (or tr/*alias-map* {})]
            (let [env (cljs.analyzer/empty-env)
                  r (doall (map (fn [form]
                                  (try (let [env (assoc env :ns
                                                            (cljs.analyzer/get-namespace cljs.analyzer/*cljs-ns*))]
                                         [form (assoc (cljs.analyzer/analyze env form) :original-form form)])
                                       (catch ThreadDeath e (throw e))
                                       (catch Throwable e
                                         (log-exception e [3 f])
                                         [form { :original-form form}])))
                                (cljs.analyzer/forms-seq res)))
                  info (swap! jsenv/*compiler* assoc-in [:cljs.analyzer/analyzed-cljs path] true)]
              (clean-fake-vars)
              {:analyze-results {:forms (map first r)
                                 :asts (map second r)
                                 :info info}})))))
    ))

