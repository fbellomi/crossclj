(ns crossclj.spawn
  (:import (org.sonatype.aether.util.version GenericVersionScheme)
           (java.io File PrintWriter BufferedInputStream BufferedOutputStream
                    FileOutputStream)
           (org.apache.lucene.index Term IndexReader MultiFields)
           (java.util.zip GZIPInputStream)
           (java.net URL)
           (org.apache.lucene.search TermQuery)
           (org.apache.commons.io FileUtils)
           (org.jdom2.output XMLOutputter)
           (org.jdom2 Element Text))
  (:require [clojure.java.io :as io]
            [crossclj.generate :as gen]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [cheshire.core :as json]
            [cemerick.pomegranate :as deps]
            [me.raynes.conch.low-level :as sh]
            [hiccup.core :as h]
            [hiccup.util :as hu])
  (:use crossclj.index))

(defn to-file [path uri] (spit path (slurp uri) :encoding "UTF-8") )
(defn to-file-json [path uri] (spit path (pr-str (json/parse-string (slurp uri) true)) :encoding "UTF-8"))

(defn gunzip
  [fi fo]
  (with-open [i (io/reader
                 (GZIPInputStream.
                  (io/input-stream fi)))
              o (PrintWriter. (io/writer fo))]
    (doseq [l (line-seq i)]
      (.println o l))))

(defn to-file-bin [path url]
  (let  [con    (-> url URL. .openConnection)
         in     (BufferedInputStream. (.getInputStream con))
         out    (BufferedOutputStream.
                 (FileOutputStream. path))
         buffer (make-array Byte/TYPE 1024)]
    (loop [g (.read in buffer)
           r 0]
      (if-not (= g -1)
        (do
          (.write out buffer 0 g)
          (recur (.read in buffer) (+ r g)))))
    (.close in)
    (.close out)
    (.disconnect con)))


(defn update-from-net []
  (to-file-bin "resources/feed.clj.gz" "http://clojars.org/repo/feed.clj.gz")
  (to-file-json "resources/clojure-artifacts.edn" "http://search.maven.org/solrsearch/select?q=g:%22org.clojure%22&rows=400&wt=json")
  (to-file-json "resources/pomegranate-artifacts.edn" "http://search.maven.org/solrsearch/select?q=a:%22pomegranate%22&rows=400&wt=json")
  (gunzip "resources/feed.clj.gz" "resources/pom-data.edn"))

(defn wrap-vec [s]
  (str \[ s \]))

(defn firstp [f seq]
  (first (filter f seq)))

(defn good-version? [v]
  (not (.contains v "SNAPSHOT")))

(def version-scheme (GenericVersionScheme.))

(defn text-abstract [s n]
  (if (< (.length s) n)
    s
    (let [pos (.indexOf s " " (dec n))]
      (if (= -1 pos)
        s
        (subs s 0 pos)))))

(defn artifact-first [group-id artifact-id]
  (if (= group-id artifact-id) (symbol artifact-id) (symbol group-id artifact-id)))

(def pardon #{"slingshot" "automat" "datomic-free"})

(def preferred '#{ robert/hooke prismatic/schema org.flatland/useful com.keminglabs/cljx })

(defn to-index-entry [m]
  (let [version (firstp good-version? (:versions m))
        group-id (:group-id m)
        ; connection (get-in m [:scm :connection])
        description (:description m "")
        ; ld (when description (.toLowerCase description))
        artifact-id (:artifact-id m)
        a1 (artifact-first group-id artifact-id)
        artifact [a1 version]
        name artifact-id
        url (or (:url m) (get-in m [:scm :url]))
        category (when description (str/trim (text-abstract description 80)))]
    (when (or (pardon artifact-id)
              (and
                version
                group-id
                artifact-id
                #_description
                url
                #_(or (.contains url "github") (and connection (.contains connection "github")))
                #_(not (.contains ld "fixme"))
                (not (.startsWith group-id "org.clojars"))))
      (assoc m :handle name :artifact artifact :name name :url url :category category))))

(defn to-index-entry-clojure [m]
  (let [artifact  [(artifact-first (:g m) (:a m)) (:latestVersion m)]
        name  (:a m)
        date (:timestamp m)
        url  (str "https://github.com/clojure/" name)]
    (assoc m :group-id (:g m) :handle name :artifact artifact :name name :url url  :sure true :date date)))

(defn to-index-entry-cemerick [m]
  (let [artifact  [(artifact-first (:g m) (:a m)) (:latestVersion m)]
        name  (:a m)
        date (:timestamp m)
        url  (str "https://github.com/cemerick/" name)]
    (assoc m :group-id (:g m) :handle name :artifact artifact :name name :url url :sure true :date date)))


(defn choose-art [a b]
  (let [version-a (.parseVersion version-scheme ((:artifact a) 1))
        version-b (.parseVersion version-scheme ((:artifact b) 1))]
    (cond
      (:sure a) a
      (:sure b) b
      (preferred ((:artifact a) 0)) a
      (preferred ((:artifact b) 0)) b
      (nil? (namespace ((:artifact a) 0))) a
      (nil? (namespace ((:artifact b) 0))) b
      (pos? (compare version-a version-b)) a
      :else b)))

(defn build-fat-index []
  (let [feed (edn/read-string (wrap-vec (slurp "resources/pom-data.edn")))
        feed-clj (get-in (edn/read-string (slurp "resources/clojure-artifacts.edn")) [:response :docs])
        feed-clj2 (get-in (edn/read-string (slurp "resources/pomegranate-artifacts.edn")) [:response :docs])
        index (filter identity (concat (map to-index-entry-clojure feed-clj) (map to-index-entry-cemerick feed-clj2) (map to-index-entry feed)))
        groups (group-by :name index)
        best (map (fn [y] (reduce choose-art y)) (vals groups))]
    best))

(defn add-preferred [index-entry]
  (let [artifact (:artifact index-entry)
        file (str gen/output-dir \/ (artifact 0) \/ (artifact 1) "/meta.edn")
        file2 (str gen/output-dir \/ (artifact 0) \/ (nth (gen/mr-versions (str (artifact 0))) 1) "/meta.edn")
        content (try (edn/read-string (slurp file)) (catch Exception _ (try (edn/read-string (slurp file2)) (catch Exception _ {}))))]
    (assoc index-entry :main-ns (:main-ns content))))

(defn find-ref [s0]
  (.search index-searcher (TermQuery. (Term. ns-target s0)) 10000))

(defn find-referrers [id]
  (let [results (do-search (str id) find-ref [ns-from ns-version])]
    results))

(defn add-referrers [m]
  (let [refs (find-referrers (first (:artifact m)))]
    (assoc m :refs (apply vector (map #(get % ns-from) refs))
             :vers (apply vector (map #(get % ns-version)  refs)))))

(defn correct [m]
  (let [m (dissoc m :scm :versions)]
    (when-not (:artifact m)
      (println m))
    (if (= (:name m) "clojure")
      (assoc m :category "Core language" :description "Core language")
      m)))

(def clojar-rep (str (System/getProperty "user.home") "/Downloads/clojars/"))

(defn ensure-pom [pom]
  (if (.exists (File. clojar-rep pom))
    true
    (do (println "Downloading" pom)
        (when-let [content (try (slurp (str "http://central.maven.org/maven2/" pom)) (catch Exception e #_(println e) nil))]
          (gen/write (str clojar-rep pom) content)
          true))))

(defn remove-js [a]
  (let [n (get-in a [:artifact 0])]
    (when-not (or (= "cljsjs" (namespace n))
                  (.startsWith (name n) "google-closure-library")) a)))

(defn add-last-modified [a]
  (let [group (str/replace (str (:group-id a)) "." "/")
        ver (get-in a [:artifact 1])
        name (:name a)
        pom-id (str group \/ name \/ ver \/ name \- ver ".pom")
        pom (File. clojar-rep pom-id)]
    (if (:category a)
      (assoc a :date (.lastModified pom))
      (if (ensure-pom pom-id)
        (let [pomx (slurp pom)
              matcher (re-matcher #"<description>(.+)</desc" pomx)
              desc (str (try (do (re-find matcher) (second (re-groups matcher))) (catch Exception _ "")))
              a (assoc a :description desc :category desc :date 0)]
            #_(println desc)
            a)
        (assoc a :category "" :description "" :date 0)))))

(defn make-fat-index []
  (println "generating fat index...")
  (let [index (filter remove-js (build-fat-index))]
    (spit "resources/content/mr-version.edn" (pr-str (doall (into {} (map (fn [m] (let [a (:artifact m)] [(str (first a)) a])) index)))) :encoding "UTF-8")
    (spit "resources/fat-index.edn" (pr-str (doall (map add-last-modified (map correct (map add-referrers (map add-preferred index)))))) :encoding "UTF-8")))

; --- generation

(defn fat-index [] (into {} (map (fn [a] [(first (:artifact a)) a]) (edn/read-string (slurp "resources/fat-index.edn" :encoding "utf-8")))))

(def classpath-string (str/join ":" (map #(subs % 5) (deps/get-classpath))))

(defn clojure-call [main & args]
  (into ["java" "-server" "-Xmx1g" "-XX:+UseCompressedOops" "-Djava.awt.headless=true" "-Dfile.encoding=utf-8" "-Dclojure.compiler.direct-linking=true" "-Dclojure.spec.skip-macros=true"
         "-cp" classpath-string
         "clojure.main" "--main" main]
        args))

(defn beautify-url [u]
  (cond
    (.startsWith u "http://") (subs u 7)
    (.startsWith u "https://") (subs u 8)
    :else u))

(defn produce-author-info [fat-index a]
  (let [art (fat-index (first a))
        url (:url art)]
    (gen/with-safe nil
                   (when (.contains url "github")
                     (let [user (cond
                                  (.contains url "//github.com") (nth (.split url "/+") 2)
                                  (.contains url "//www.github.com") (nth (.split url "/+") 2)
                                  (.contains url ".github.io") (nth (.split url "\\.") 0)
                                  (.startsWith url "github.com/") (nth (.split url "/+") 1)
                                  :else "òòò")
                           info (json/parse-string
                                  (slurp (str "https://api.github.com/users/" user "?client_id=XXX&client_secret=XXX"))
                                  true)]
                       (gen/write (str "resources/content/ns/" (a 0) \/ (a 1) "/author.html")
                                  (h/html [:div.stit "OWNER"]
                                          [:div [:a {:href (:html_url info) :target "_blank"} [:img.ownerpic {:src (str (:avatar_url info) "&s=160")}]]]
                                          (when-let [u (or (:name info) user)]
                                            [:div [:a {:href (:html_url info) :target "_blank"} (h/h u)]])
                                       (map (fn [k] (when-let [n (k info)] [:div n])) [:company :location :email])
                                       (when-let [u (:blog info)]
                                         [:div [:a {:href (if (.startsWith u "http") u (str "http://" u)) :target "_blank"} (beautify-url u)]])
                                       )))))))

(defn generate [fi artifacts]
  (println "generate: " artifacts)
  (let [p (apply sh/proc (apply clojure-call "crossclj.generate" (map str (flatten artifacts))))]
    (future (sh/stream-to-out p :out))
    (future (sh/stream-to-out p :err))
    (println (sh/exit-code p)))
  (dorun (map #(produce-author-info fi %) artifacts)))

(def long-prjs '#{org.clojure/core.cache org.clojure/clojure org.clojure/clojurescript org.clojure/core.typed
                      org.clojure/clojure-contrib })

(def excluded-prjs (conj long-prjs 'clojure-watch 'cljs-contrib 'jark 'org.clojure/google-closure-library))

(defn init-index [] (.close (gen/writer-create)))

(defn index-projects [index]
  (let [w (reset! gen/writer (gen/writer-append))]
    (dorun (map (fn [x]
                  (let [artifact (:artifact x)
                        description (:description x)]
                    (gen/index-name (str (artifact 0) \space (artifact 1)) artifact :prj description)))
                index))
    (.forceMerge w 1)
    (.close w)))

(defn generate-all []
  (io/delete-file "resources/content/index/write.lock" true)
  (io/delete-file "timeout.txt" true)
  (io/delete-file "exceptions.txt" true)
  (io/delete-file "log.txt" true)
  (io/delete-file "ord.txt" true)
  (init-index)
  (let [fi (fat-index)
        index (vals fi)
        pass1 (filter #(not (excluded-prjs (first %))) (map :artifact (sort-by (comp - count :refs) index)))]
    (dorun (map #(generate fi [(gen/latest-artifact %)]) long-prjs))
    (dorun (map #(generate fi %) (partition-all 12 pass1)))
    (index-projects index)))

(def files-to-keep #{"exceptions.txt" "full-artifacts.edn" "other" "pomsync" "project.clj"
                    "resources" "src" "sync" "target" "timeout.txt" "ord.txt" "buildjs" "crawler"
                    ".gitignore" ".idea" "crossclj.iml" "exesync" "oinksync" ".lein-repl-history"})

(defn delete-unknown []
  (dorun (map #(FileUtils/deleteQuietly (File. %)) (filter (complement files-to-keep) (map #(.getName %) (.listFiles (File. "."))))))
  (FileUtils/deleteQuietly (File. "resources/pom-data.edn")))

(def sitemap-namespace (org.jdom2.Namespace/getNamespace "http://www.sitemaps.org/schemas/sitemap/0.9"))

(def serializer (XMLOutputter.))

(defn write-sitemap2 []
  (let [index-reader (IndexReader/open index-dir)]
    (when-let [fields (MultiFields/getFields index-reader)]
      (when-let [terms (.terms fields doc-id)]
        (let [enum (.iterator terms nil)
              ids (loop [ids ()]
                    (if-let [b (.next enum)]
                      (recur (conj ids (.utf8ToString b)))
                      ids))]
          (doseq [[i ids0] (map vector (iterate inc 0) (partition-all 50000 ids))]
            (let [doc (org.jdom2.Document.)
                  root (Element. "urlset" sitemap-namespace)]
              (.setRootElement doc root)
              (doseq [n ids0]
                (let [url (Element. "url" sitemap-namespace)
                      loc (Element. "loc" sitemap-namespace)
                      nn (str/split n #",")]
                  (when (= 4 (count nn))
                    (.addContent root url)
                    (.addContent url loc)
                    (.addContent loc (Text. (str "https://crossclj.info/fun/" (nn 2) \/ (hu/url-encode (nn 3)) ".html"))))))
              (gen/write (str "resources/site/sitemap2-" i ".xml") (.outputString serializer doc))))
          (.close index-reader))))))

(defn write-sitemap1 []
  (let [fat-index (vals (fat-index))
        doc (org.jdom2.Document.)
        root (Element. "urlset" sitemap-namespace)]
    (.setRootElement doc root)
    (doseq [n (filter :main-ns fat-index)]
      (do
        (let [url (Element. "url" sitemap-namespace)
              loc (Element. "loc" sitemap-namespace)
              art (:artifact n)
              main-ns (:main-ns n)]
          (.addContent root url)
          (.addContent url loc)
          (.addContent loc (Text. (str "https://crossclj.info/ns/" (art 0) "/latest/" main-ns ".html"))))))
    (gen/write "resources/site/sitemap1.xml" (.outputString serializer doc))))

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn write-sitemap3 []
  (let [files (map #(str/replace % #"/[0-9]([^/]+)/" "/latest/") (filter #(not (.endsWith % "menu.html")) (map #(.getPath %) (walk gen/output-dir-doc #".*\.html"))))
        doc (org.jdom2.Document.)
        root (Element. "urlset" sitemap-namespace)]
    (.setRootElement doc root)
    (doseq [f files]
      (do
        (let [url (Element. "url" sitemap-namespace)
              loc (Element. "loc" sitemap-namespace)]
          (.addContent root url)
          (.addContent url loc)
          (.addContent loc (Text. (str "https://crossclj.info/" (subs f 18)))))))
    (gen/write "resources/site/sitemap-docs.xml" (.outputString serializer doc))))

(defn shell [& args]
  (let [p (apply sh/proc args)]
        (future (sh/stream-to-out p :out))
        (future (sh/stream-to-out p :err))
        (println args (sh/exit-code p))))

(defn run-all []
  (update-from-net)
  (shell "./pomsync")
  (make-fat-index)
  #_(shell "rm" "-Rf" "resources/content/ns" "resources/content/doc")
  (shell "rm" "-Rf" "resources/content/index2/*")
  (shell "mv" "resources/content/index/*" "resources/content/index2")
  (load "/crossclj/generate")
  (generate-all)
  (load "/crossclj/index")
  (make-fat-index)
  (load "/crossclj/generate")
  (write-sitemap1)
  (write-sitemap2)
  (write-sitemap3)
  #_(load "/crossclj/server")
  (delete-unknown)
  #_(shell "./sync")
  (shell "curl" "https://crossclj.info/reloadXXX")
  (println "Finished!")
  (Thread/sleep (* 20 60 1000))
  (load "/crossclj/spawn")
  (recur))

(defn daemon []
  (init-index)
  (shell "mkdir" "resources/content/index2")
  (shell "mv"  "resources/content/index2/*" "resources/content/index" )
  (load "/crossclj/server")
  (.start (Thread. #(run-all))))

