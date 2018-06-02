(ns crossclj.index
  (:import (org.apache.lucene.store FSDirectory)
           (java.io File)
           (org.apache.lucene.search TopDocs IndexSearcher)
           (org.apache.lucene.index IndexReader)
           (org.apache.lucene.analysis.standard StandardAnalyzer)
           (org.apache.lucene.util Version))
  (:require [clojure.string :as str]))

(def index-path "resources/content/index")
(def index-dir (FSDirectory/open (File. index-path)))

(def analyzer (StandardAnalyzer. Version/LUCENE_4_10_1))

(def doc-id "did")
(def doc-n "dn")

(def doc-apps "apps")

(def doc-type "type")
(def doc-text "text")
(def doc-source "source")
(def doc-source-start "sourcest")
(def doc-vtype "vtype")
(def doc-args "args")
(def doc-value "val")
(def doc-art "art")
(def doc-docs "dz")

(def ns-target "targ")
(def ns-version "ver")
(def ns-from "from")

(def type-env (atom {}))

(def old-index-reader (when (bound? #'index-reader) index-reader))

(def implicit-deps '[leiningen org.clojure/clojure org.clojure/data.xml bultitude stencil reply
                     slingshot cheshire clj-http classlojure robert/hooke com.cemerick/pomegranate pedantic
                     org.clojure/clojurescript])

(def index-reader (try (IndexReader/open index-dir) (catch Exception e (println e))))

(when old-index-reader (println "closing old index") (.close old-index-reader))

(def index-searcher (try (IndexSearcher. index-reader) (catch Exception e (println e))))

(defn found-docs [^TopDocs hits from to]
  (let [scoreDocs (.scoreDocs hits)
        to2 (min to (alength scoreDocs))]
    (map #(.doc index-searcher (.doc (aget scoreDocs %))) (range from to2))))

(defn do-search [s fun keys]
  (let [hits (fun s)
        n (.totalHits hits)]
    (map (fn [d] (reduce #(assoc %1 %2 (.get d %2)) {} keys)) (found-docs hits 0 n))))

(defn pp-type [f]
  (if (and (sequential? f) (<= 3 (count f)) (symbol? (first f)) (= (name (first f)) "All"))
    (str "(All " (pr-str (second f)) "\n   " (str/join "\n   " (str/split (pp-type (nth f 2)) #"\n" )) ")")
    (if (and (sequential? f) (<= 3 (count f)) (symbol? (first f)) (= (name (first f)) "IFn"))
              (str "(IFn " (str/join "\n   " (map pr-str (rest f))) ")")
              (pr-str f))))

(defn clean-type [f]
    (-> (pp-type f)

        (str/replace #"(clojure\.core\.typed/)|(clojure\.lang\.)|(java\.lang\.)" "")
        (str/replace "(U " "(\u222a ")
        (str/replace "(I " "(\u2229 ")
        (str/replace "(All " "(\u2200 ")
        (str/replace "(IFn " "(\u03bb ")
        (str/replace " -> " " \u2192 ")
        (str/replace "[-> " "[\u2192 ")
        ))

(defn chardigit [s]
  (apply str (filter #(Character/isLetterOrDigit %) s)))

(defn fun-token [n ns]
  (str n \, ns))

(defn min-namespace-name
  ([s0 n]
   (loop [s s0 p 0]
     (if (<= (count s) n)
       s
       (let [p1 (.indexOf s "." p)]
         (if (= p1 -1)
           s
           (let [s1 (str (subs s 0 (+ p 1)) (subs s p1))]
             (recur s1 (+ 2 p))))))))
  ([s0]
   (min-namespace-name s0 24)))




