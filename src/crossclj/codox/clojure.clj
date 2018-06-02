(ns crossclj.codox.clojure
  "Read raw documentation information from Clojure source directory."
  (:use [crossclj.codox.utils :only (assoc-some update-some correct-indent)])
  (:require [clojure.string :as str]
            [crossclj.analyze-ns :as ans]))

(defn- sorted-public-vars [namespace]
  (->> (ns-interns namespace)
       (vals)
       (sort-by (comp :name meta))))

(defn- proxy? [var]
  (re-find #"proxy\$" (-> var meta :name str)))

(defn- macro? [var]
  (:macro (meta var)))

(defn- special-type? [var]
  (:clojure.core.typed/special-type (meta var)))

(defn- multimethod? [var]
  (instance? clojure.lang.MultiFn (var-get var)))

(defn- protocol? [var]
  (let [value (var-get var)]
    (try (and (map? value) (:on-interface value)) (catch ThreadDeath e (throw e)) (catch Throwable e false))))

(defn- protocol-method? [var]
  (:protocol (meta var)))

(defn- protocol-methods [protocol vars]
  (filter #(= protocol (:protocol (meta %))) vars))

(defn- var-type [var]
  (cond
   (macro? var)       :macro
   (multimethod? var) :multimethod
   (protocol? var)    :protocol
   (special-type? var) :type
   :else              :var))

(defn- read-var [vars var]
  (-> (meta var)
      (select-keys [:name :file :line :arglists :doc :dynamic :author
                    :added :deprecated  :clojure.core.typed/special-type :forms])
      (assoc :public (.isPublic var))
      (update-some :doc correct-indent)
      (assoc-some  :type    (var-type var)
                   :members (seq (map (partial read-var vars)
                                      (protocol-methods var vars))))))

(defn- read-publics [namespace]
  (let [vars (sorted-public-vars namespace)]
    (->> vars
         (remove proxy?)
         (remove protocol-method?)
         (map (partial read-var vars))
         (sort-by (comp str/lower-case :name)))))

(defn read-ns [namespace]
  (try
    (-> (find-ns namespace)
        (meta)
        (assoc :name namespace)
        (assoc :publics (read-publics namespace))
        (update-some :doc correct-indent)
        (list))
    (catch Exception e
      (println
       (format "Could not generate clojure documentation for %s - root cause: %s %s"
               namespace
               (.getName (class e))
               (.getMessage e)))
      #_(.printStackTrace e)
      (ans/log-exception e [8 namespace])
      (list {:name namespace}))))

