(ns crossclj.gzip
  "Ring gzip compression."
  (:require [clojure.java.io :as io])
  (:import (java.io InputStream
                    Closeable
                    File
                    ByteArrayOutputStream)
           (java.util.zip GZIPOutputStream)))

(defn- accepts-gzip?
  [req]
  (if-let [accepts (get-in req [:headers "accept-encoding"])]
    ;; Be aggressive in supporting clients with mangled headers (due to
    ;; proxies, av software, buggy browsers, etc...)
    (re-seq
      #"(gzip\s*,?\s*(gzip|deflate)?|X{4,13}|~{4,13}|\-{4,13})"
      accepts)))

;; Set Vary to make sure proxies don't deliver the wrong content.
(defn- set-response-headers
  [headers]
  (if-let [vary (get headers "vary")]
    (-> headers
      (assoc "Vary" (str vary ", Accept-Encoding"))
      (assoc "Content-Encoding" "gzip")
      (dissoc "Content-Length")
      (dissoc "vary"))
    (-> headers
      (assoc "Vary" "Accept-Encoding")
      (assoc "Content-Encoding" "gzip")
      (dissoc "Content-Length"))))

(def ^:private supported-status? #{200, 201, 202, 203, 204, 205 403, 404})

(defn- unencoded-type?
  [headers]
  (if (headers "content-encoding")
    false
    true))

(defn- supported-type?
  [resp]
  (let [{:keys [body]} resp]
    (or (string? body)
        (seq? body)
        (instance? InputStream body)
        (and (instance? File body)
             (re-seq #"(?i)\.(htm|html|css|js|json|xml)" (pr-str body))))))

(def ^:private min-length 859)

(defn- supported-size?
  [resp]
  (let [{body :body} resp]
    (cond
      (string? body) (> (count body) min-length)
      (seq? body) (> (count body) min-length)
      (instance? File body) (> (.length body) min-length)
      :else true)))

(defn- supported-response?
  [resp]
  (let [{:keys [status headers]} resp]
    (and (supported-status? status)
         (unencoded-type? headers)
         (supported-type? resp)
         (supported-size? resp))))

(defn- compress-body [body]
  (let [bas (ByteArrayOutputStream.)]
    (with-open [out (GZIPOutputStream. bas)]
      (if (seq? body)
        (doseq [string body] (io/copy (str string) out))
        (io/copy body out)))
    (when (instance? Closeable body)
      (.close body))
    (.close bas)
    (.toByteArray bas)))

(defn- gzip-response
  [resp]
  (-> resp
    (update-in [:headers] set-response-headers)
    (update-in [:body] compress-body)))

(defn test-gzip [handler]
  (fn [request]
    (handler (if (accepts-gzip? request)
               (assoc request :gzip true)
               request))))

(defn wrap-gzip
  "Middleware that compresses responses with gzip for supported user-agents."
  [handler]
  (fn [req]
    (if (:gzip req)
      (let [resp (handler req)]
        (if (supported-response? resp)
          (gzip-response resp)
          resp))
      (handler req))))

