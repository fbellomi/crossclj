(ns crossclj.codox.main
  "Main namespace for generating documentation"
  (:use [crossclj.codox.utils :only (ns-filter)])
  (:require [crossclj.codox.clojure :as clj]
            [cljs.env :as env]
            [crossclj.codox.clojurescript :as cljs]
            [crossclj.codox.html :as html]))

(defn choose-reader [path]
  (cond
    (or (.endsWith path ".clj") (.endsWith path ".cljc")) (fn [_ ns] (clj/read-ns (symbol ns)))
    (.endsWith path ".cljs") (fn [path ns] (cljs/read-ns path ns))
    :else (throw (Error. ^String path))))

(defn read-namespaces-all [nss2]
  (env/ensure (doall (sort-by :name
                              (mapcat (fn [[path ns]] ((choose-reader path) path ns)) nss2)))))

(defn generate-all [output-dir artifact nss2 x]
  (html/write-docs {:output-dir output-dir
                                :namespaces (concat (read-namespaces-all nss2) x)} artifact))

