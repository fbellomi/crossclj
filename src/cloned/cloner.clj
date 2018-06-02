(ns cloned.cloner
  (:import (java.io File))
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(require '[clojure.tools.namespace.find :as f]
         '[clojure.tools.namespace.move :as m])

(def targets (f/find-namespaces-in-dir (File. "/xxx/crossclj/src/clojure")))

(def names (into {} (map (fn [t] [t (symbol (str "cloned." (name t)))]) targets)))

(defn- ns-file-name [sym]
  (str (-> (name sym)
           (str/replace "-" "_")
           (str/replace "." File/separator))
       ".clj"))

(defn move-ns-file
  [old-sym new-sym source-path]
  (let [old-file (io/file source-path (ns-file-name old-sym))
        new-file (io/file source-path (ns-file-name new-sym))]
    #_(println old-file)
    (when (.exists old-file)
      (println old-sym new-sym)
      (.mkdirs (.getParentFile new-file))
      (io/copy old-file new-file))))

(defn- update-file
  "Reads file as a string, calls f on the string plus any args, then
  writes out return value of f as the new contents of file. Does not
  modify file if the content is unchanged."
  [file f & args]
  (when (.exists file)
    (let [old (slurp file)
          new (str (apply f old args))]
      (when-not (= old new)
        (spit file new)))))

(defn replace-targets [s]
  (reduce-kv m/replace-ns-symbol s names))

(defn move-ns
  [old-sym new-sym source-path]
  (move-ns-file old-sym new-sym source-path)
  (update-file (File. source-path (ns-file-name new-sym)) replace-targets))


(defn clone-nss []
  (doall (map (fn [ns]
                (move-ns ns (names ns) "/xxx/crossclj/src"))
              targets)))
