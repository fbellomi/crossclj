(defproject crossclj "0.1.0-SNAPSHOT"
  :description "Cross reference between clojars clojure artifacts"
   :dependencies [[org.clojure/clojure "1.9.0-alpha8"]
                 [ring "1.4.0"]
                 [compojure "1.4.0"]
                 [org.clojure/core.cache "0.6.4"]
                 [org.clojure/core.memoize "0.5.7"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/clojurescript "1.9.36"]
                 [org.codehaus.jsr166-mirror/jsr166y "1.7.0"]
                 [leiningen "2.5.3" :exclusions [cheshire]]
                 [clojail "1.0.6"]
                 [cheshire "5.5.0"]
                 [commons-io "2.4"]
                 [hiccup "1.0.5"]
                 [enlive "1.1.6"]
                 [org.jdom/jdom "2.0.2"]
                 [clj-time "0.9.0"]
                 [bk/ring-gzip "0.1.1"]
                 [me.raynes/conch "0.5.0"]
                 [com.taoensso/timbre "4.1.1"]
                 [org.flatland/classlojure "0.7.1"]
                 [com.cemerick/pomegranate "0.3.0"]
                 [org.pegdown/pegdown "1.4.2"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.apache.lucene/lucene-core "4.10.1"]
                 [org.apache.lucene/lucene-queryparser "4.10.1"]
                 [org.apache.lucene/lucene-analyzers-common "4.10.1"]
                 [com.fasterxml.jackson.core/jackson-core "2.5.3"]
                                  [com.fasterxml.jackson.dataformat/jackson-dataformat-smile "2.5.3"]
                                  [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor "2.5.3"]]

:jvm-opts ["-Xmx600M" "-server" "-XX:+UseCompressedOops" "-XX:-OmitStackTraceInFastThrow"]

:ring {:handler crossclj.server/app}

:main crossclj.server

:profiles { :crawler { :main crossclj.spawn
                       :jvm-opts ["-Xmx300M" "-server" "-XX:+UseCompressedOops"
                                  "-XX:-OmitStackTraceInFastThrow" "-Dclojure.compiler.direct-linking=true"]
                       }})

; cloned dependencies:
;[org.clojure/tools.analyzer "0.6.1"]
;[org.clojure/tools.analyzer.jvm "0.6.5" :exclusions [org.clojure/tools.analyzer]]
;[org.clojure/tools.reader "0.8.12"]
