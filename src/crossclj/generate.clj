(ns crossclj.generate
  (:import (clojure.lang DynamicClassLoader
                         Namespace Var)
           (java.io File StringReader FileNotFoundException PushbackReader
                    IOException FileWriter)
           (java.net URL)
           (java.lang.reflect Method)
           (org.apache.lucene.index IndexWriter IndexWriterConfig
                                    IndexWriterConfig$OpenMode)
           (org.apache.lucene.util Version)
           (org.apache.lucene.document Document StringField Field$Store
                                       StoredField TextField)
           (java.util.concurrent TimeoutException TimeUnit)
           (org.pegdown PegDownProcessor Extensions)
           (org.sonatype.aether.util.version GenericVersionScheme))
  (:require [clojure.java.io :as io]
            [crossclj.bultitude :as b]
            [hiccup.core :as h]
            [hiccup.util :as hu]
            [clojure.walk :as walk]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [crossclj.analyze-ns :as ans]
            [cemerick.pomegranate :as deps]
            [cemerick.pomegranate.aether :as aether]
            [classlojure.core :as load]
            [clojure.tools.namespace]
            [cloned.clojure.tools.reader :as reader]
            [clojail.core :as jail]
            [crossclj.codox.main :as docs])
  (:use crossclj.index)
  (:gen-class))

(defn debug [x] (println x) x)

(defn find-ns1 [sym]
  (try (require sym)(catch Throwable _ nil))
  (create-ns sym))

; --- markdown

(def markdown-processor (PegDownProcessor. Extensions/ALL))

(defn md-to-html [s]
  (let [s (-> s
              (.replaceAll "```([a-z])" "\n```$1")
              (.replace "\r" ""))]
    (-> (.markdownToHtml markdown-processor s)
        (str/replace "<br/>" " ")
        (str/replace #"href=\"[^h#][^\"]+\"" "href=\"#\"")
        (str/replace #"src=\"[^h#][^\"]+\"" "src=\"/0.png\"")
        )))

; --- tag readers (copied from clojre.core)

(defn data-reader-urls []
  (enumeration-seq
   (.. Thread currentThread getContextClassLoader
       (getResources "data_readers.clj"))))

(defn data-reader-var [sym]
  (intern (create-ns (symbol (namespace sym)))
          (symbol (name sym))))

(defn load-data-reader-file [mappings ^java.net.URL url]
  (with-open [rdr (clojure.lang.LineNumberingPushbackReader.
                   (java.io.InputStreamReader.
                    (.openStream url) "UTF-8"))]
    (binding [*file* (.getFile url)]
      (let [new-mappings (read rdr false nil)]
        (when (not (map? new-mappings))
          (throw (ex-info (str "Not a valid data-reader map")
                          {:url url})))
        (reduce
         (fn [m [k v]]
           (when (not (symbol? k))
             (throw (ex-info (str "Invalid form in data-reader file")
                             {:url url
                              :form k})))
           (let [v-var (data-reader-var v)]
             (when (and (contains? mappings k)
                        (not= (mappings k) v-var))
               (throw (ex-info "Conflicting data-reader mapping"
                               {:url url
                                :conflict k
                                :mappings m})))
             (assoc m k v-var)))
         mappings
         new-mappings)))))

(defn load-data-readers []
  (alter-var-root #'reader/*data-readers*
                         (let [data-readers (data-reader-urls)]
                           (fn [mappings]
                             (reduce load-data-reader-file
                                     mappings data-readers)))))

; --- lucene

(defn writer-create []
  (let [writer-config (IndexWriterConfig. Version/LUCENE_4_10_1 analyzer)]
    (.setOpenMode writer-config IndexWriterConfig$OpenMode/CREATE)
    (IndexWriter. index-dir writer-config)))

(defn writer-append []
  (let [writer-config (IndexWriterConfig. Version/LUCENE_4_10_1 analyzer)]
    (.setOpenMode writer-config IndexWriterConfig$OpenMode/CREATE_OR_APPEND)
    (IndexWriter. index-dir writer-config)))


(defn analyze-ns [ns path]
  (let [ cljs? (.endsWith path ".cljs")
         analysis (if cljs? (ans/analyze-ns-cljs ns path) (ans/analyze-ns ns path))
         results (get-in analysis [:analyze-results :asts])
         forms (get-in analysis [:analyze-results :forms])
         info (get-in analysis [:analyze-results :info])]
    [results forms info]))

(defrecord VarApp [var literal line column])
(defrecord MethodApp [class method literal line column])
(defrecord VarDef [var literal line column trueline])
(defrecord ProtocolEx [protocol target literal line column])
(defrecord ProtocolMethodEx [protocol target var literal line column])

(defrecord Refer [type source var line column])

(defn find-sym [a s]
  (when-let [a0 (first a)]
    (if (= a0 s)
      a0
      (if (sequential? a0)
        (find-sym a0 s)
        (recur (rest a) s)))))

(defn fapps [ns as ast top-form]
  (let [as (cond  ; recur
             (sequential? ast)
               (reduce #(fapps ns  %1 %2 top-form) as ast)

             (and (map? ast) (:children ast))
               (reduce (fn [a k] (fapps ns a (get-in ast [k] k) top-form)) as (:children ast))

             :else as)]
    ; collect
    (let [as (if (and (map? ast)
                   (= :var (:op ast)))
               (let [m (meta (ast :form))
                     line (:line m)]
                 (if line (conj as (VarApp.
                                     (or (:var ast)         ; clj
                                         (let [v (get-in ast [:info :name])
                                               namespace (namespace v)
                                               v (if (and (= (str ns) namespace) (not= (str ns) "cljs.core") (find-var (symbol "clojure.core" (name v)))) (symbol "cljs.core" (name v)) v)]
                                           v))              ; cljs
                                     (str (:form ast))
                                     line
                                     (:column m)))
                          as))
                 as)

          as (if (and (map? ast)
                   (= :def (ast :op))
                   (var? (ast :var)))
               (let [n (:name ast)
                     msym (meta (find-sym top-form n))
                     line (:line (meta top-form))]
                 (if line (conj as (VarDef.
                                     (ast :var)
                                     (str n)
                                     line
                                     (:column msym 1)
                                     (:line msym 1)))
                          as))
               as)

          as (if (and (map? ast)
                   (= :def (ast :op))
                   (map? (ast :var)))
               (let [n (:name ast)
                     msym (meta (get-in ast [:var :form]))
                     line (:line (meta top-form))]
                       #_(println n (:line (meta top-form)))
                 (if line (conj as (VarDef.
                                     n
                                     (name n)
                                     line
                                     (:column msym 1)
                                     (:line msym 1)))
                          as))
               as)

          as (if (and (map? ast)
                      (or (and (= (ast :op) :const)
                               (= (ast :tag) Class))
                          (and (#{:static-call :instance-call} (ast :op) )
                               (:class ast))))
               (let [form (and (:raw-forms ast) (:zform (first (:raw-forms ast))))
                     m (meta form)
                     line (:line m)]
                 (if (and form line)
                   (conj as (MethodApp.
                                     (:class ast)
                                     (:method ast)
                                     (str form)
                                     line
                                     (:column m)))
                   as))
               as)

          as (if (and (map? ast)
                      (= (ast :op) :new))
               (let [clss (get-in ast [:class :form])
                     form (:form ast)
                     literal (and (:raw-forms ast) (:zform (first (:raw-forms ast))))
                     m (or (meta literal) (meta form))
                     line (:line m)]
                 (if line (if-not literal
                                 (conj as (MethodApp. clss nil (str (second form)) line (when-let [c (:column m)] (+ 5 c))))
                                 (conj as (MethodApp. clss nil (str literal) line (:column m))))
                               as))
               as)

          as (if (and (map? ast)
                      (= (ast :op) :const)
                      (= (ast :tag) Class)
                      (:val ast)
                      (:line (meta (:form ast))))
                 (conj as (MethodApp.
                            (:val ast)
                            nil
                            (str (:form ast))
                            (:line (meta (:form ast)))
                            (:column (meta (:form ast)))))
               as)

          as (if (and (map? ast)
                      (ast :tag))
               (let [tag-class (:tag ast)
                     tag (:tag (meta (:form ast)))
                     line (:line (meta tag))]
                 (if line (conj as (MethodApp.
                                tag-class                   ;class
                                nil                         ;method
                                (str tag)                   ;literal
                                line
                                (:column (meta tag)))) as))
              as)

          as (if (and (map? ast)
                      (:raw-forms ast)
                      (map? (first (:raw-forms ast))))
               (let [ fx (first (:raw-forms ast))
                        f (:zform fx)
                        m (meta f)
                        z (:zvar fx)
                        line (:line m)]
                   (if (and z line) (conj as (VarApp. z (str f) line (:column m))) as))
                 as)]
      as)))

(defn safe1 [v f]
  (try (f) (catch ThreadDeath e (throw e)) (catch Throwable e (println "safe: " (.toString e)) #_(.printStackTrace e) v)))

(defn safe1-nil [v f]
  (try (if-let [r (f)] r v) (catch ThreadDeath e (throw e)) (catch Throwable e (println "safe: " (.toString e)) #_(.printStackTrace e) v)))

(defmacro with-safe [v & body]
  `(safe1  ~v (fn [] ~@body)))

(defmacro with-safe-nil [v & body]
  `(safe1-nil  ~v (fn [] ~@body)))

(defn collect-func-apps [ns asts]
  (reduce (fn [a b] (fapps ns a b (get-in b [:original-form]))) () asts))

(def repositories
  (merge cemerick.pomegranate.aether/maven-central
     {"clojars" "http://clojars.org/repo/"
      "snappy" "https://oss.sonatype.org/content/repositories/snapshots/"
      "sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}))

(def jar-uri-format #"/repository((?:/[^/]+)+)/([^/]+)/([^/]+)/(.+).jar")

(defn to-artifact [jar-uri]
  (if (instance? File jar-uri)
    (to-artifact (.getPath jar-uri))
    (when-let [ms (re-find jar-uri-format jar-uri)]
      (let [authority (str/replace (subs (ms 1) 1) \/ \.)
            package (ms 2)
            version (ms 3)
            id (if (= authority package) (symbol package) (symbol authority package))]
        [id version]))))

(def mr-versions (edn/read-string (slurp "resources/content/mr-version.edn")))

(defn most-recent [artifact]
  (if-let [mr (mr-versions (str (artifact 0)))]
    mr
    artifact))

(declare implicit-art-map)

(defn build-ns-artifact-map
  ([art]
   (build-ns-artifact-map art implicit-art-map))
  ([art x]
   (reduce merge x
           (map (fn [file]
                  (if-let [artifact (to-artifact file)]
                    (reduce (fn [m ns] (assoc m ns (most-recent artifact)))
                            {}
                            (b/file->namespaces nil file))
                    {}))
                (with-safe () (let [deps (aether/resolve-dependencies
                                           :coordinates [art]
                                           :repositories repositories)]
                                (aether/dependency-files deps)))))))

(def implicit-art-map (->
                        (reduce merge (map #(build-ns-artifact-map (mr-versions (str %)) {}) (filter #(mr-versions (str %)) implicit-deps)))
                        (assoc 'cljs.core (mr-versions "org.clojure/clojurescript"))))

(defn try-add-dependencies
  [& args]
  (let [deps (try (apply aether/resolve-dependencies args) (catch ThreadDeath e (throw e)) (catch Throwable e (ans/log-exception e [10 (:coordinates args)]) ()))]
    (doseq [artifact-file (aether/dependency-files deps)]
      (deps/add-classpath artifact-file))
    deps))

(defn with-artifact [artifact f fail]
  (let [temp-classloader (DynamicClassLoader. (.getContextClassLoader (Thread/currentThread)))]
    (load/with-classloader temp-classloader
                           (try (try-add-dependencies
                                      :coordinates [artifact]
                                      :repositories repositories)
                                (catch ThreadDeath e (throw e))
                                (catch Throwable e
                                  (ans/log-exception e [4 artifact])
                                  (println e) (fail e artifact)))
                           (f))))

(def output-dir "resources/content/ns")
(def output-dir-doc "resources/content/doc")

(defn link-class [v this-ns]
  (if (var? v)
    (let [meta (meta v)
              namespace (if (:ns meta)
                          (ns-name (:ns meta))
                          (symbol (namespace (symbol (subs (str v) 2)))))]
      (with-safe "a" (if (= (ns-name this-ns) namespace) "p" "a")))
    (if (and (symbol? v) (not (find-ns v)))
      (with-safe "a" (let [na (namespace v)] (if (or (nil? na) (= (str (ns-name this-ns)) na)) "p" "a")))
      "a")))

(defn link [ns-art v this-ns]
  (if (var? v)
    (let [meta (meta v)
          namespace (if (:ns meta)
                      (ns-name (:ns meta))
                      (symbol (namespace (symbol (subs (str v) 2)))))
          artifact (ns-art namespace)]
      (if-not artifact
        (do #_(println "failed: " namespace " in " this-ns) "#")
        (str "/ns/" (artifact 0) \/ (artifact 1) \/ namespace ".html#_" (hu/url-encode (:name meta "?")))))
    (if (symbol? v)
      (if (find-ns v)
        (let [artifact (ns-art v)]
          (if-not artifact
            (do #_(println "failed: " namespace " in " this-ns) "#")
            (str "/ns/" (artifact 0) \/ (artifact 1) \/ v ".cljs.html")))
        (let [namespace (symbol (or (namespace v) this-ns))
              artifact (ns-art namespace)]
          (if-not artifact
            (do #_(println "failed: " namespace " in " this-ns) "#")
            (str "/ns/" (artifact 0) \/ (artifact 1) \/ namespace ".cljs.html#_" (hu/url-encode (name v))))))
      (let [namespace (ns-name v)                           ; v instanceof namespace
            artifact (ns-art (symbol namespace))]
        (if-not artifact
          (do #_(println "failed: " v " in " this-ns) "#")
          (str "/ns/" (artifact 0) \/ (artifact 1) \/ namespace ".html"))))))

(defn link-usage [v]
  (if (symbol? v)
    (str "/fun/" (namespace v) ".cljs/" (hu/url-encode (name v)) ".html") ; clojurescript
    (let [meta (meta v)
          namespace (if (:ns meta)
                      (ns-name (:ns meta))
                      (symbol (namespace (symbol (subs (str v) 2)))))]
      (str "/fun/" namespace \/ (hu/url-encode (:name meta "X")) ".html"))))

(defn base-url [class-name]
  (cond
    (.startsWith class-name "java") ["docs.oracle.com/javase/8/docs/api/" ".html"]
    (.startsWith class-name "clojure.lang") ["github.com/clojure/clojure/tree/master/src/jvm/" ".java"]
    :else nil))

(defn link-method [class method]
  (if class
    (let [class-name (.getName class)
          url (base-url class-name)
          class-path (.replace class-name \. \/)]
      (if method
        (let [method (str method)
              ^Method method-obj (first (filter #(= (.getName %) method) (.getMethods class)))
              method-str (.toGenericString method-obj)
              method-id (subs method-str (+ 1 (.indexOf method-str (str class-name \.)) (count class-name)))
              throw-pos (.indexOf method-id " throws")
              method-id (if (= throw-pos -1) method-id (subs method-id 0 throw-pos))
              method-id (str/replace method-id "," "-")
              method-id (str/replace method-id "(" "-")
              method-id (str/replace method-id ")" "-")
              method-id (str/replace method-id "[]" ":A")
              method-id (str/replace method-id "..." ":A")]
          #_(println class-name "*" method-str "*" method-id)
          (str (url 0) class-path (url 1) "#" method-id))
        (str (url 0) class-path (url 1))))
    "#"))

(defn class-to-var [c]
  (when c
    (if (symbol? c)
      (try (class-to-var (Class/forName (str c)))
           (catch ThreadDeath e (throw e))
           (catch Throwable _e nil))
      (if (instance? Var c)
        c
        (if (instance? Class c)
          (let [name (.getName c)
                pos (.lastIndexOf name ".")]
            (when (> pos -1)
              (let [ns-name (subs name 0 pos)
                    var-name (subs name (+ 1 pos))]
                (when-let [ns (find-ns1 (symbol ns-name))]
                  (let [var (ns-resolve ns (symbol var-name))]
                    (when (var? var)
                      var))))))
          (println "failed class-to-var " c))))))

(def escape-html identity)

(defn line->html
  ([text fas-line ns-art ns]
   (line->html () 0 (if (empty? text) " " text) fas-line ns-art ns))
  ([result col text fas-seq ns-art ns]
   #_(println col text (when-not (empty? fas-seq) (first fas-seq)))
   (if (empty? fas-seq)
     (reverse (conj result (escape-html text)))
     (let [el (first fas-seq)
           pos (- (:column el) col)
           length (count (str (:source el)))
           end (+ pos length)]
       (if (and (>= pos 0) (<= end (count text))) ;; fails because of reader macros
         (let [pre (subs text 0 pos)
               middle (subs text pos end)
               post (subs text end)]
           (cond
             (and (= (:type el) :apply))
             (let [ v (:var el)
                    lv (link ns-art v ns)
                    lc (escape-html middle)
                    cla (link-class v ns)
                    res1 (conj result
                              (escape-html pre)
                              #_[:a {:href lv} lc] (str "\"ç@" cla lc "ç@" lv \"))]
               #_(println el)
               (if-let [vn (and (not= lv "#")
                                (or (:name (meta v))
                                    (and (instance? Namespace v) "")
                                    (and (symbol? v) (find-ns v) "")
                                    (and (symbol? v) (namespace v) (name v))))]
                 (if (.endsWith middle (str vn))
                   (recur res1 (+ col end) post (rest fas-seq) ns-art ns)
                   (recur result col text (rest fas-seq) ns-art ns))
                 (do #_(println el) (recur result col text (rest fas-seq) ns-art ns))))

             #_(= (:type el) :extend)
             #_()

             (= (:type el) :extend-method)
             (let [ v (:method el)
                    lv (link ns-art v ns)
                    lc (escape-html middle)
                    res1 (conj result
                              (escape-html pre)
                              #_[:a {:href lv} lc] (str "\"ç@x" lc "ç@" lv \"))]
               (if-let [vn (and (not= lv "#") (or (:name (meta v)) (and (instance? Namespace v) "") (and (symbol? v) (namespace v) (name v))))]
                 (if (.endsWith middle (str vn))
                   (recur res1 (+ col end) post (rest fas-seq) ns-art ns)
                   (recur result col text (rest fas-seq) ns-art ns))
                 (do #_(println el) (recur result col text (rest fas-seq) ns-art ns))))

             (= (:type el) :def)
             (let [ v (:var el)
                    lv (link-usage v)
                    lc (escape-html middle)
                    res1 (conj result
                              (escape-html pre)
                              #_[:a {:href lv} lc] (str "\"ç@d" lc "ç@" lv \"))]
               (if-let [vn (or (:name (meta v)) (and (symbol? v) v))]
                 (if (.endsWith middle (name vn))
                   (do #_(println "* " vn) (recur res1 (+ col end) post (rest fas-seq) ns-art ns))
                   (do #_(println "1 " vn) (recur result col text (rest fas-seq) ns-art ns)))
                 (do #_(println "2 " v) (recur result col text (rest fas-seq) ns-art ns))))

             (and (= (:type el) :static)
                  (= (:source el) middle)
                  (instance? Class (first (:var el)))
                  (base-url (.getName (first (:var el)))))
             (let [[class method] (:var el)
                   lv (link-method class method)
                   lc (escape-html middle)
                   res1 (conj result
                              (escape-html pre)
                              #_[:a {:href   lv
                                   :target "_blank"} lc] (str "\"çç@" lc "ç@" lv \"))]
               (recur res1 (+ col end) post (rest fas-seq) ns-art ns))

             (and (= (:type el) :static)
                  (= (:source el) middle)
                  (class-to-var (first (:var el))))
             (let [lv (link ns-art (class-to-var (first (:var el))) ns)
                   lc (escape-html middle)
                   res1 (conj result
                              (escape-html pre)
                              #_[:a {:href lv} lc] (str "\"ç@a" lc "ç@" lv \"))]
               (recur res1 (+ col end) post (rest fas-seq) ns-art ns))
             :else (recur result col text (rest fas-seq) ns-art ns)))
         (do #_(when (= :def (:type el)) (println "3 " (:source el))) (recur result col text (rest fas-seq) ns-art ns)))))))

(def no-index (volatile! #{}))

(defn line+defs [html fas-line text]
  (into html
        (when (seq fas-line)
          (let [text (chardigit text)]
            (map (fn [d] (let [v (:var d)
                                 av (if (symbol? v) (name v) (str (:name (meta v))))]
                             #_[:a {:id av}] (if (.contains text (chardigit av))
                                               (str "\"ç@_" av \")
                                               (do #_(println av) (vswap! no-index conj d) nil)))) fas-line)))))

(defn clean-target [s]
  (str/replace s #"(clojure\.lang\.)|(java\.lang\.)|(java\.util\.)" ""))

(defn extension-id [d]
  (str (:name (meta (:method d))) \~ (clean-target (or (:target d) "?")) \~ (:name (meta (:var d)))))

(defn line+defs-prot [html fas-line]
  (into html (map (fn [d]
                    #_(println d)
                    (let [av (extension-id d)]
                          #_[:a {:id av}] (str "\"ç@_" av \"))) fas-line)))

(defn filter-as [fs]
  (if (empty? fs)
    ()
    (let [s (first fs)]
      (if (= :as s)
        (recur (rest (rest fs)))
        (conj (filter-as (rest fs)) s)))))

(defn collect-sym [ns syms cljs?]
  (filter identity
    (flatten
      (map
        (fn [sym]
          (if (symbol? sym)
            (when-let [var (if cljs? (symbol (str ns) (str sym)) (with-safe nil (ns-resolve ns sym)))]
              (let [m (meta sym)]
                (VarApp. var (str sym) (:line m) (:column m))))
            (if (sequential? sym)
              (collect-sym ns sym cljs?))))
        (filter-as syms)))))

(defn import-entry [fs cljs?]
  (if (empty? fs)
    ()
    (let [s (first fs)
          r (rest fs)]
      (if (sequential? s)
        (let [p (first s)]
          (concat (import-entry r cljs?)
            (filter :class
              (map (fn [s]
                     (let [m (meta s)]
                       (MethodApp. (with-safe nil (ns-resolve *ns* (symbol (str p \. s)))) nil (str s) (:line m) (:column m))))
                   (rest s)))))
        (if (symbol? s)
          (if-let [c (with-safe nil (ns-resolve *ns* s))]
            (let [m (meta s)]
              (conj (import-entry r cljs?) (MethodApp. c nil (str s) (:line m) (:column m))))
            (import-entry r cljs?))
          (import-entry r cljs?))))))

(defn- prefix-spec?
  [form]
  (and (sequential? form)
       (symbol? (first form))
       (not-any? keyword? form)
       (< 1 (count form))))

(defn- option-spec?
  [form]
  (and (sequential? form)
       (symbol? (first form))
       (or (keyword? (second form))
           (= 1 (count form)))))

(defn- deps-from-libspec [prefix form cljs?]
  (cond
    (and (sequential? form) (= 'quote (first form)))
      (deps-from-libspec prefix (second form) cljs?)
    (prefix-spec? form)
          (mapcat (fn [f] (deps-from-libspec
                               (symbol (str (when prefix (str prefix "."))
                                            (first form)))
                               f
                               cljs?))
                      (rest form))
	(option-spec? form)
    (let [val (deps-from-libspec prefix (first form) cljs?)]
      (concat val (collect-sym (:var (first val)) (rest form) cljs?)))
  (symbol? form)
    (let [ns (symbol (str (when prefix (str prefix ".")) form))
          m (meta form)
          var (if cljs? ns (with-safe nil (find-ns1 ns)))]
      (list (VarApp. var (str form) (:line m) (:column m))))
  (keyword? form)  ; Some people write (:require ... :reload-all)
          ()
	:else
          (do (println "UNK")())))

(defn- deps-from-ns-form [form cljs?]
  (mapcat #(deps-from-libspec nil % cljs?) (rest form)))


(defn ns-entry [fs cljs?]
  (condp = (first fs)
    :use (deps-from-ns-form fs cljs?)
    :use-macros (deps-from-ns-form fs false)
    :require (deps-from-ns-form fs cljs?)
    :require-macros (deps-from-ns-form fs false)
    :import (import-entry (seq fs)  cljs?)
    :refer-clojure (collect-sym (find-ns 'clojure.core) (rest fs)  cljs?)
    :gen-class ()
    'use (deps-from-ns-form fs  cljs?)
    'use-macros (deps-from-ns-form fs false)
    'require (deps-from-ns-form fs  cljs?)
    'require-macros (deps-from-ns-form fs false)
    'import (import-entry (seq fs) cljs?)
    'refer-clojure (collect-sym (find-ns 'clojure.core) (rest fs) cljs?)
    'gen-class ()
    (do (println "UNK " (first fs))())))

(defn collect-ns-ref [nsf cljs?]
  (let [a (try
            (concat
              (if (and (sequential? nsf) (= 'ns (first nsf)))
                (do
                  (when (> (count nsf) 2) (let [s (nth nsf 2)] (when (instance? String s)
                                                                 (try (alter-meta! (find-ns1 (nth nsf 1)) assoc :doc s) (catch Exception _)))))
                  (mapcat #(ns-entry % cljs?) (filter sequential? nsf))
                  )
                ())
              (if (and (sequential? nsf) (#{'use 'require} (first nsf)))
                (deps-from-ns-form nsf cljs?)
                ()))
            (catch Exception e (println e nsf) (.printStackTrace e)))]
    a))

(def excluded-ns0 [])

(defn excluded-ns [s]
  (let [s0 (str s)]
    (some #(.startsWith s0 (str %)) excluded-ns0)))

(defn collect-protocols-fun-defs [ns]
  (map (fn [v] (let [m (meta v)] (->VarDef v (:name m) (:line m) (:column m) (:line m))))
              (filter #(:protocol (meta %)) (vals (ns-map (find-ns ns))))))

(defn protocol-method-var [protocol-var method-sym]
  (when protocol-var
    ((ns-map (:ns (meta protocol-var))) method-sym)))

(defn collect-type-extension0 [ns form target]
  (loop [p form protocol nil fas ()]
    (if (seq p)
      (let [k (first p)]
        (if (list? k)
          (recur (next p) protocol (let [method (first k)
                                       m (meta method)]
                                   (if (and protocol (var? protocol) (:on @protocol))
                                     (if-let [pvar (protocol-method-var protocol method)]
                                       (conj fas (->ProtocolMethodEx protocol target pvar method (:line m) (:column m)))
                                       fas)
                                     fas)))
          (let [protocol (when k (ns-resolve ns k))]
            (recur (next p)
                   protocol
                   (let [m (meta k)]
                     (conj fas (->ProtocolEx protocol target k (:line m) (:column m))))))))
      fas)))

(defn collect-type-extension [ns form]
  (when-let [p (next form)]
    (collect-type-extension0 ns (next p) (first p))))

(defn collect-record [ns form]
  (when-let [p (next form)]
    (collect-type-extension0 ns (next (next p)) (first p))))

(defn collect-reify [ns form]
  (when-let [p (next form)]
    (collect-type-extension0 ns p "$")))

(defn collect-protocol-extension [ns form]
  (when-let [p (next form)]
    (when-let [protocol (ns-resolve ns (first p))]
      (loop [p (next p) target nil fas ()]
        (if (seq p)
          (let [k (first p)]
            (if (list? k)
              (recur (next p) target (let [method (first k)
                                           m (meta method)
                                           pvar (protocol-method-var protocol method)]
                                       (if pvar
                                         (conj fas (->ProtocolMethodEx protocol target pvar method (:line m) (:column m)))
                                         fas)))
              (let [target (if k (str k) "nil")]
                (recur (next p)
                       target
                       (let [m (meta k)]
                         (conj fas (->ProtocolEx protocol target k (:line m) (:column m))))))))
          fas)))))

(defn collect-protocol-extensions [ns forms]
  (with-safe nil
    (let [fas (volatile! ())]
      (walk/prewalk (fn [f] (when (and (list? f) (symbol? (first f)))
                              (let [ff (first f)]
                                (cond
                                  (= ff 'extend-protocol) (vswap! fas into (collect-protocol-extension ns f))
                                  (= ff 'reify) (vswap! fas into (collect-reify ns f))
                                  ('#{deftype defrecord} ff) (vswap! fas into (collect-record ns f))
                                  (= ff 'extend-type) (vswap! fas into (collect-type-extension ns f))
                                  ))) f) forms)
      @fas)))

(defn collect-func-apps-ns [ns path]
  (let [[asts forms info]
          (if (excluded-ns ns)
            [() () ()]
            (try (analyze-ns ns path)
                 (catch ThreadDeath e (throw e))
                 (catch Throwable e (println "ERRX " ns " " (.toString e))
                                    #_(.printStackTrace e)
                                    (ans/log-exception e [5 path])
                                    [() () ()])))
        cljs? (.endsWith path ".cljs")
        fas1 (collect-func-apps ns asts)
        fas3 (collect-protocols-fun-defs ns)
        fas2 (mapcat #(collect-ns-ref % cljs?) forms)
        fas4 (collect-protocol-extensions ns forms)
        fas (concat fas1 fas2 fas3 fas4)]
    [(distinct
       (map
         (fn [fa]
           (let [line (- (:line fa) 1)
                 column (- (:column fa) 1)]
             (cond
               (instance? VarApp fa)
               (do #_(println fa)
                 (let [actual-string (:literal fa)
                       actual (:var fa)]
                   (if-not (or (var? actual) (instance? Namespace actual) (symbol? actual))
                     (println fa actual-string)
                     (->Refer :apply actual-string actual line column))))

               (instance? MethodApp fa)
               (->Refer :static (:literal fa) [(:class fa) (:method fa)] line column)

               (instance? VarDef fa)
               (let [var (:var fa)
                     actual-string (if (symbol? var) (name var) (:name (meta var)))]
                 (assoc (->Refer :def actual-string var line column)
                   :trueline (- (:trueline fa) 1)))

               (instance? ProtocolEx fa)
               (assoc (->Refer :extend (str (:literal fa)) (:protocol fa) line column)
                 :target (:target fa))

               (instance? ProtocolMethodEx fa)
               (assoc (->Refer :extend-method (str (:literal fa)) (:protocol fa) line column)
                 :target (:target fa) :method (:var fa))

               :else (throw (InternalError.))
               )))
         (filter :line fas))) info]))

(def writer (atom nil))

(defn index-project [user useds]
  (doseq [used useds]
    (let [doc (Document.)]
      (.add doc (StoredField. ns-from user))
      (.add doc (StringField. ns-target (first used) Field$Store/NO))
      (.add doc (StoredField. ns-version (str (second used))))
      (.addDocument @writer doc))))

(defn index-name [name art type docs]
  (let [doc (Document.)
        n (str name)
        n0 (subs n (+ 1 (.indexOf n "/")))]
    (.add doc (StoredField. doc-type (clojure.core/name type)))
    (.add doc (StringField. doc-n n Field$Store/YES))
    (.add doc (StoredField. doc-art (str (art 0) \| (art 1))))
    (when docs
      (.add doc (TextField. doc-docs docs Field$Store/NO)))
    (doseq [v (str/split n0 #"[\./ ]")]
      (.add doc (StringField. doc-value (str/lower-case (chardigit v)) Field$Store/NO)))
    (.addDocument @writer doc)))

(defn index-def-name [doc name art type text args]
  (let [n (str name)
        n0 (subs n (+ 1 (.indexOf n "/")))]
    (.add doc (StoredField. doc-type (clojure.core/name type)))
    (.add doc (StringField. doc-n n Field$Store/YES))
    (when text
      (.add doc (TextField. doc-text text Field$Store/YES)))
    (when args
      (let [args (if (= 'quote (first args)) (second args) args)]
        (.add doc (StoredField. doc-args (str/join "," (sort-by count (map (fn [s] (str/replace s "," " ")) args)))))))
    (.add doc (StoredField. doc-art (str (art 0) \| (art 1))))
    (doseq [v (str/split n0 #"[\./ ]")]
      (let [field (StringField. doc-value (str/lower-case (chardigit v)) Field$Store/NO)]
        (.add doc field)))))

(defn first-sub
  ([s] (let [start (.indexOf s "(")] (if (= -1 start) (dec (.length s)) (first-sub start 0 s))))
  ([pos i s]
   (if (= pos (.length s))
     (dec pos)
     (let [c (.charAt s pos)
           i (cond (= c \() (inc i)
                  (= c \)) (dec i)
                  :else i)]
        (if (zero? i)
          pos
          (recur (inc pos) i s))))))

(defn safe-subvec [v f t]
  (let [c (count v)]
    (if (or (< c f) (< c t))
      (do #_(println c f t) v)
      (subvec v f t))))

(defn to-index [art ns fas lines info]
  (when-not (or (and (= (name ns) "clojure.core") (not= (art 0) 'org.clojure/clojure))
                (and (= (name ns) "cljs.core") (not= (art 0) 'org.clojure/clojurescript)))
    (let [lines (into [] lines)
          cljs? (some? info)
          docs
          (with-safe-nil
            (loop [fas fas docs () doc nil]
              (if-let [fa (first fas)]
                (do
                  (condp = (:type fa)

                    :def
                    (if (@no-index fa)
                      (recur (rest fas) docs doc)
                      (let [doc (Document.)
                            nx (str ns \/ (:source fa))
                            nx2 (str ns (when cljs? ".cljs") \/ (:source fa))
                            mmeta (with-safe-nil (get-in info [:cljs.analyzer/namespaces ns :defs (symbol (:source fa))]) (meta (find-var (symbol nx))))
                            last-source (if-let [l (:line (some #(#{:def :extend-method} (:type %)) (next fas)))] l (count lines))
                            source (apply str (mapcat #(apply str %) (safe-subvec lines (:line fa) last-source)))
                            vsym (symbol (str ns) (str (:source fa "zzz")))]
                        #_(println nx2)
                        (index-def-name doc nx2 art :var (:doc mmeta) (:arglists mmeta))
                        (.add doc (StoredField. doc-source (subs source 0 (+ 1 (first-sub source)))))
                        (with-safe-nil (when-let [vtype (@type-env vsym)]
                                         #_(println vtype)
                                         (.add doc (StoredField. doc-vtype (clean-type vtype)))))
                        (when-let [line (:line fa)]
                          (.add doc (StoredField. doc-source-start (str line))))
                        (.add doc (StringField. doc-id (str (art 0) \, (art 1) \, ns (when cljs? ".cljs") \, (:source fa)) Field$Store/YES))
                        (recur (rest fas) (conj docs doc) doc)))

                    ;                (assoc (->Refer :extend-method (str (:literal fa)) (:protocol fa) line column)
                    ;                 :target (:target fa) :method (:var fa))

                    ; (defrecord Refer [type source var line column])


                    :extend-method
                    #_()
                    #_(println fa)
                    (let [doc (Document.)
                          extend-id (extension-id fa)                                   ;todo
                          nx2 (str ns (when cljs? ".cljs") \/ extend-id)
                          mmeta (with-safe-nil {} ((keyword (:source fa)) (:sigs @(:var fa))))
                          last-source (if-let [l (:line (some #(#{:def :extend-method} (:type %)) (next fas)))] l (count lines))
                          source (apply str (mapcat #(apply str %) (safe-subvec lines (:line fa) last-source)))
                          vsym (symbol (str ns) extend-id)]
                      (index-def-name doc nx2 art :var (:doc mmeta) (:arglists mmeta))
                      (.add doc (StoredField. doc-source (subs source 0 (+ 1 (first-sub source)))))
                      (with-safe-nil (when-let [vtype (@type-env vsym)]
                                       (.add doc (StoredField. doc-vtype (clean-type vtype)))))
                      (when-let [line (:line fa)]
                        (.add doc (StoredField. doc-source-start (str line))))
                      (.add doc (StringField. doc-id (str (art 0) \, (art 1) \, ns (when cljs? ".cljs") \, extend-id) Field$Store/YES))
                      (.add doc (StringField. doc-apps (let [m (meta (:var fa))] (fun-token (:name m) (ns-name (:ns m)))) Field$Store/NO))
                      (recur (rest fas) (conj docs doc) doc))

                    :apply
                    (let [v (:var fa)]
                      (if (or (var? v) (instance? Namespace v))
                        (let [meta (meta v)
                              namespace (if-let [nm (:ns meta)]
                                          (ns-name nm)
                                          (let [k (symbol (subs (str v) 2))]
                                            (if-let [z (namespace k)]
                                              (symbol z)
                                              (symbol (str v)))))
                              namespace (if cljs? (str namespace ".cljs") namespace)
                              name (:name meta)]
                          (when (and doc (or (not (var? v)) (not (with-safe nil (:on-interface @v)))))
                            (.add doc (StringField. doc-apps (fun-token name namespace) Field$Store/NO)))
                          (recur (rest fas) docs doc))
                        ; symbol v
                        (let [namespace (str (namespace v) (when cljs? ".cljs"))
                              name (name v)]
                          (when doc
                            (.add doc (StringField. doc-apps (fun-token name namespace) Field$Store/NO)))
                          (recur (rest fas) docs doc))))

                    (recur (rest fas) docs doc)))
                docs)))]
      (index-name ns art :ns (:doc (meta (find-ns ns))))
      (.addDocuments @writer docs))))

(defn distinct-by
  [ff coll]
    (let [step (fn step [xs seen]
                   (lazy-seq
                    ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (let [v (ff f)]
                          (if (contains? seen v)
                            (recur (rest s) seen)
                            (cons f (step (rest s) (conj seen v)))))))
                     xs seen)))]
      (step coll #{})))

(defn ns->html
  ([ns ns-art path]
   (let [text (try (slurp (io/resource path)) (catch Exception _ (println "NOT FOUND " path " : " ns) "nil"))]
     (ns->html ns
               text
               (collect-func-apps-ns ns path)
               ns-art)))
  ([ns source [fas0 info] ns-art]
   (vreset! no-index #{})
   (let [fas (group-by :line (filter #(not= :def (:type %)) fas0))
         defs (filter #(= :def (:type %)) fas0)
         fas2 (group-by :trueline (distinct-by :source defs))
         fas3 (group-by :line (filter #(= (:type %) :extend-method) fas0))
         fas1 (group-by :trueline defs)
         lines (seq (map-indexed (fn [line text]
                                   (-> (str text \newline)
                                       (line->html (sort-by :column (concat (fas1 line) (fas line))) ns-art ns)
                                       (line+defs (fas2 line) text)
                                       (line+defs-prot (fas3 line))
                                       ))
                                 (str/split-lines source)))]
     (to-index (ns-art ns) ns (sort-by :line (sort-by :column fas0)) lines info)
     lines)))

(defn write [file content]
  (when (and file content)
    (.mkdirs (.getParentFile (File. file)))
    (spit file content :encoding "UTF-8")))

(defn to-file [ns ns-art path]
    (let [artifact (ns-art ns)
          cljs? (.endsWith path ".cljs")
          file (str output-dir \/ (artifact 0) \/ (artifact 1) \/ ns (when cljs? ".cljs") ".html")
          content (h/html (ns->html ns ns-art path))]
      (write file content)))

(defn nss-tree [prefix artifact nss2]
  (reduce (fn [m [path ns]]
                  (let [cljs? (.endsWith path ".cljs")
                        href (str prefix (artifact 0) \/ (artifact 1) \/ ns (when cljs? ".cljs") ".html")
                        segs (conj (vec (str/split (name ns) #"\.")) (if cljs? :cljs :clj))]
                    (assoc-in m segs href))) {} nss2))

(defn compact-entry [[k m]]
  (if (map? m)
    (let [kk (first (keys m))]
      (if (and (= 1 (count m)) (string? kk) (< (count (str k kk)) 20))
        [(str k \. kk) (m kk)]
        [k m]))
    [k m]))

(defn tree-markup [[k v]]
  (if (map? v)
    (let [cljs? (:cljs v)
          clj? (:clj v)
          name (min-namespace-name k)
          head-clj (when clj? [:div [:a.nse {:href clj?} name]])
          head-cljs (when cljs? [:div [:a.nse {:href cljs?} name] [:span.cljs "s"]])
          head-no (when-not (or clj? cljs?) [:div.trno name])
          children (sort-by first (dissoc v :clj :cljs))
          els (map (fn [k] (tree-markup k)) children)]
      (list head-clj head-cljs head-no (when-not (empty? children) [:div.trin els])))
    v))

(defn compact-tree [tree]
  (into {} (map (fn [[k v]] (let [v2 (if (map? v) (compact-tree v) v)] (compact-entry [k v2]))) tree)))

(defn to-namespaces-html [dir prefix artifact nss2 path1 path2]
  (let [file (str dir \/ (artifact 0) \/ (artifact 1) "/menu.html")
        tree (compact-tree (nss-tree prefix artifact nss2))]
    (write file
      (h/html [:div.ns
         (list [:a.proja {:href path1} "project"] [:br] [:br])
         (list [:a.proja {:href path2} "docs index"] [:br] [:br])
         (when-not (empty? tree) [:div.stit "NAMESPACES"])
         (map tree-markup tree)]))))

(defn preferred-ns [ns] (let [k (str ns)] (- (count k) (if (or (.endsWith k "core") (.endsWith k "core.cljs")) 100 0))))

(defn thread [f] (let [t (Thread. f)] (.setDaemon t true) (.start t)))

(defmacro with-timeout [millis & body]
    `(let [future# (future ~@body)]
      (try
        (.get future# ~millis TimeUnit/MILLISECONDS)
        (catch TimeoutException _#
          (do
            (future-cancel future#)
            nil)))))

(defn safe [f]
  (fn [x] (try (f x) (catch ThreadDeath e (throw e)) (catch Throwable e (.printStackTrace e) e))))

(defn find-artifact [name]
  (mr-versions (str name)))

(defn get-main-ns [artifact]
  (try (:main-ns (edn/read-string (slurp (str output-dir \/ (artifact 0) \/ (artifact 1) "/meta.edn") :encoding "utf-8")))
       (catch Exception _ "pippo")))

(defn encode-dep [dep]
  (if-let [art (mr-versions (str (if (= (name dep) (namespace dep)) (name dep) dep)))]
    (if-let [namespace (get-main-ns art)]
      (str "\"ç@a" dep "ç@" (str "/ns/" (art 0) \/ (art 1) \/ namespace ".html") \")
      "no-main")
    (str dep)))

(defn replace-map [m s0]
  (loop [s s0 v (sort-by #(- (.length (% 0))) (seq m))]
    (if-let [f (first v)]
      (recur (.replace s (str \[ (f 0) \space) (str \[ (f 1) \space)) (rest v))
      s)))

(defn project-map [project-form]
  (apply hash-map (drop 3 project-form)))

(defn get-deps [form]
  (cond
    (map? form) (let [d (concat (:dependencies form) (:dev-dependencies form) (:plugins form))
                      k (mapcat get-deps (vals (dissoc form :dependencies :dev-dependencies :plugins)))] (concat d k))
    (sequential? form) (mapcat get-deps form)
    :else ()))

(defn distinct-version [deps]
  (seq (into {} (map #(vec (take 2 %)) (reverse (filter #(and (vector? %) (<= 2 (count %))) deps))))))

(defn decorate-project [artifact s project]
  (try
    (let [ deps (distinct-version (concat (get-deps project) (with-safe nil (keys (dissoc
                                                                                    (aether/resolve-dependencies :coordinates [artifact] :repositories repositories)
                                                                                    artifact)))))
           m (into {} (map #(let [k (first %)] [(str k) (encode-dep k)]) deps))]

      #_(println (first deps))
      (index-project (str (artifact 0)) (map (fn [[a v]] [(if (= (name a) (namespace a)) (name a) (str a))  v]) deps))
      (replace-map m s))
    (catch Exception e (println "decorate: " e) (ans/log-exception e [6 artifact]) s)))

(defn full-coord [art]
  (let [a (art 0)]
    (if (namespace a)
      (str a)
      (str a \/ a))))

(defn process-md [artifact jar-uri name]
  (let [project-uri (str "jar:" jar-uri "!/META-INF/leiningen/" (full-coord artifact) \/ name)
        file (str (artifact 0) \/ (artifact 1) \/ name)
        file2 (str file ".html")
        stream (try (.getInputStream (.openConnection (URL. project-uri))) (catch IOException _ nil))]
    (when stream
      (let [content (slurp stream :encoding "utf-8")]
        (write (str output-dir \/ file2) (md-to-html content))
        #_(write (str output-dir \/ file) content)))))

(defn get-project-form [jar-uri]
  (let [project-uri (str "jar:" jar-uri "!/project.clj")
         stream (.getInputStream (.openConnection (URL. project-uri)))
         project-all (slurp stream :encoding "utf-8")
         stream (PushbackReader. (StringReader. project-all))
         project (first (filter #(and (sequential? %) (= 'defproject (first %))) (take-while identity (repeatedly #(try (read stream) (catch Exception _e nil))))))]
     (or project ())))

(defn write-project [artifact jar-uri]
  (try (let [file (str (artifact 0) \/ (artifact 1) "/project.clj.html")
             project-uri (str "jar:" jar-uri "!/project.clj")
             stream (or (with-safe nil (io/input-stream (URL. project-uri)))
                        (when (= "org.clojure" (namespace (artifact 0))) (io/input-stream (URL. (str "https://raw.githubusercontent.com/clojure/" (name (artifact 0)) "/master/project.clj")))))
             project-all (with-safe "" (slurp stream :encoding "utf-8"))
             stream (PushbackReader. (StringReader. project-all))
             project (first (filter #(and (sequential? %) (= 'defproject (first %))) (take-while identity (repeatedly #(try (read stream) (catch Exception _e nil))))))

             zz (decorate-project artifact project-all (project-map project))]
         (when project
           (write (str output-dir \/ file) zz))
         (str "/ns/" file))
       (catch Exception e (when-not (instance? FileNotFoundException e) (.printStackTrace e)) (str "/ns/" (artifact 0) \/ (artifact 1) "/project.clj.html"))))

(defmacro with-timeout [time & body]
  `(jail/thunk-timeout (fn [] ~@body) ~time))

(defn to-nsx [nss2]
  (map (fn [[p ns]] (if (.endsWith p ".cljs") (str ns ".cljs") ns)) nss2))

(defn append-file [file s]
  (let [w (FileWriter. file true)]
    (.write w (str s \newline))
    (.close w)))

(def ^:dynamic *timeout* 4000000)

(def version-scheme (GenericVersionScheme.))

(defn choose-art [a b]
  (let [version-a (.parseVersion version-scheme (a 1))
        version-b (.parseVersion version-scheme (b 1))]
    (if (pos? (compare version-a version-b)) a b)))

(defn best-deps [deps]
  (let [groups (group-by first deps)]
    (map (fn [y] (reduce choose-art y)) (vals groups))))

(defn process-artifact [artifact]
  (println artifact)
  #_(append-file "log.txt" artifact)
  (reset! writer (writer-append))
  (let [result (let [jar-name (str \/ (name (artifact 0)) \- (artifact 1) ".jar")
                     failure (str output-dir \/ (artifact 0) \/ (artifact 1) "/failure")]
                 (with-safe ()
                   (try (with-timeout *timeout*
                     (with-artifact artifact
                        (fn []
                          (if-let [jar-uri (some #(when (.contains %  jar-name) %) (deps/get-classpath))]
                            (do
                              (try (try-add-dependencies
                                       :coordinates (best-deps (get-deps (project-map (get-project-form jar-uri))))
                                       :repositories repositories)
                                   (load-data-readers)
                                   (catch ThreadDeath e (throw e))
                                   (catch Throwable e
                                     (ans/log-exception e [11 artifact])
                                     nil))
                              (let [nss2 (b/file->namespaces2 (File. (.getPath (URL. jar-uri))))
                                    nss (map second nss2)
                                    ns-art (build-ns-artifact-map artifact)
                                    ns-art (reduce #(assoc %1 %2 artifact) ns-art nss)]
                                (let [file (str output-dir \/ (artifact 0) \/ (artifact 1) "/meta.edn")
                                      project-path (write-project artifact jar-uri)
                                      doc-path (str "/doc/" (artifact 0) \/ (artifact 1) \/ "index.html")]
                                  (write file (pr-str
                                                {:main-ns (or (first (sort-by preferred-ns (to-nsx nss2))) (when project-path "project.clj"))}))
                                  (to-namespaces-html output-dir "/ns/" artifact nss2 project-path doc-path)
                                  (to-namespaces-html output-dir-doc "/doc/" artifact nss2 project-path doc-path))
                                (process-md artifact jar-uri "README.md")
                                (binding [cljs.analyzer/*cljs-warnings* {}]
                                  (let [otherns
                                        (safe1 nil #(docs/read-namespaces-all (filter (fn [x] (.endsWith (first x) ".cljs")) nss2)))]
                                    (cloned.clojure.tools.analyzer.env/with-env (cloned.clojure.tools.analyzer.jvm/global-env)
                                                                         (with-bindings {clojure.lang.Compiler/LOADER (clojure.lang.RT/makeClassLoader)}
                                                                           (doseq [n nss] (create-ns n))
                                                                           (doseq [[path ns] (filter #(io/resource (% 0)) nss2)]
                                                                             (try (to-file ns ns-art path) (catch ThreadDeath e (throw e)) (catch Throwable e (.printStackTrace e) (ans/log-exception e [7 artifact]))))
                                                                            (safe1 nil #(docs/generate-all (str output-dir-doc \/ (artifact 0) \/ (artifact 1)) artifact (filter (fn [x] (or (.endsWith (first x) ".clj") (.endsWith (first x) ".cljc"))) nss2) otherns))))))
                                nss))
                            (do (println (ex-info "can't find jar" {:artifact artifact})) ())))
                        (fn [_ e]
                          (write failure (str e)))))
                                 (catch TimeoutException e (println "*** timeout" artifact) (ans/log-exception e ["timeout" artifact]) (append-file "timeout.txt" artifact) (write failure (str e)))
                                 (catch Exception e (println e) (ans/log-exception e ["global" artifact])))))]
    (.close @writer)
    (println result)
    result))

(defn latest-artifact [id]
  (mr-versions (str id)))

(defn -main [& args]
  (append-file "ord.txt" args)
  (binding [*timeout* (if (= 2 (count args)) Integer/MAX_VALUE 500000)]
    (doall (map (safe (fn [s] (process-artifact [(symbol (first s)) (second s)]))) (partition 2 args))))
  (System/exit 0))

