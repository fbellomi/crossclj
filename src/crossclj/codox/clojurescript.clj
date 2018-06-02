(ns crossclj.codox.clojurescript
  "Read raw documentation information from ClojureScript source directory."
  (:use [crossclj.codox.utils :only [assoc-some update-some correct-indent]])
  (:require [clojure.java.io :as io]
            [cljs.analyzer :as an]
            [cljs.env :as env]
            [crossclj.analyze-ns :as ans]
            [clojure.string :as str]))

(defn- protocol-methods [protocol vars]
  (let [proto-name (name (:name protocol))]
    (filter #(if-let [p (:protocol %)] (= proto-name (name p))) vars)))

(defn- var-type [opts]
  (cond
   (:macro opts)           :macro
   (:protocol-symbol opts) :protocol
   :else                   :var))

(defn- read-var [file vars var]
  (-> var
      (select-keys [:name :line :arglists :doc :dynamic :added :deprecated #_:doc/format])
      (update-some :doc correct-indent)
      (update-some :arglists second)
      (assoc-some  :type    (var-type var)
                   :public (when-not (:private var) true)
                   :members (map (partial read-var file vars)
                                 (protocol-methods var vars)))))


(defn- namespace-vars [analysis namespace]
  (->> (get-in analysis [::an/namespaces namespace :defs])
       (map (fn [[name opts]] (assoc opts :name name)))))

(defn- read-publics [analysis namespace file]
  (let [vars (namespace-vars analysis namespace)]
    (->> vars
         (remove :protocol)
         (remove :anonymous)
         (map (partial read-var file vars))
         (sort-by (comp str/lower-case :name)))))

(defn- analyze-file [file]
  (binding [an/*analyze-deps* true]
    (an/analyze-file file)))

(defn read-ns [file ns]
  (try
    (analyze-file (io/resource file))
    (let [analysis @env/*compiler*]
      (vals (apply merge
                         (for [namespace [ns]
                               :let [doc (get-in analysis [::an/namespaces namespace :doc])]]
                           {namespace
                             {:name    (symbol (str namespace ".cljs"))
                              :cljs    true
                              :publics (read-publics analysis namespace file)
                              :doc     (correct-indent doc)}}))))
    (catch Exception e
      (println
       (format "Could not generate clojurescript documentation for %s - root cause: %s %s"
               file
               (.getName (class e))
               (.getMessage e)))
      (.printStackTrace e)
      (ans/log-exception e [9 namespace])
      (list {:cljs true :name (symbol (str ns ".cljs"))}))))

