(ns crossclj.server
  (:import (org.apache.lucene.util Version)
           (java.util LinkedHashSet))
  (:import (java.util.prefs Preferences)
           (java.util Date TimerTask Timer)
           (java.io File ByteArrayInputStream FileWriter PrintWriter)
           (java.net URLEncoder URLDecoder)
           (org.apache.lucene.index Term)
           (org.apache.lucene.search TermQuery
                                     BooleanQuery BooleanClause PrefixQuery
                                     BooleanClause$Occur FuzzyQuery)
           (org.apache.lucene.queryparser.classic QueryParser)
           (java.text SimpleDateFormat)
           (org.sonatype.aether.util.version GenericVersionScheme)
           )
  (:require
    [ring.adapter.jetty :as jetty]
            [clj-time.format]
            [cloned.cheshire.core :as json]
            [clojure.core.cache :as cache]
            [net.cgrand.enlive-html :as html]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as timbre]
            [ring.util.time :as time]
            [clojure.edn :as edn]
            [hiccup.util :as hu]
            [hiccup.element :as el]
            )
  (:use crossclj.index
        ring.middleware.resource
        ring.middleware.file
           ring.middleware.params
           ring.util.response
           ring.middleware.content-type
           ring.middleware.not-modified
           ring.middleware.session
           crossclj.gzip
           compojure.core
           )
  (:import (org.eclipse.jetty.server Server))
  )

(timbre/refer-timbre)
(timbre/merge-config! { :level :debug :output-fn (fn [data] (str (force (:msg_ data)))) })

; --- lucene

(defn search-fun [s]
  (let [sym (if (string? s) (symbol s) s)
        n (name sym)
        nas (namespace sym)
        token (fun-token n nas)
        term (Term. doc-apps token)
        query (TermQuery. term)]
    (.search index-searcher query 10000)))

(defn find-symbol [s0]
  (.search index-searcher (TermQuery. (Term. doc-n (str s0))) 1))

(defn search-symbol [s0]
  (let [s1 (str s0)
        query (BooleanQuery.)]
    (doseq [s (str/split s1 #"[ \.]")]
      (let [term (Term. doc-value (chardigit s))
            tq (TermQuery. term)
            pq (PrefixQuery. term)
            fq (FuzzyQuery. term 1 3)]
        (.setBoost tq 5)
        (.setBoost pq 1)
        (.setBoost fq 0.3)
        (.add query (BooleanClause. pq BooleanClause$Occur/SHOULD))
        (.add query (BooleanClause. tq BooleanClause$Occur/SHOULD))
        (.add query (BooleanClause. fq BooleanClause$Occur/SHOULD))
        ))
    (.search index-searcher query 250)))

; --- global etag handler

(let [etag-storage "etag"
      prefs (Preferences/userNodeForPackage String)
      etag-value (.getInt prefs etag-storage 0)]
  (.putInt prefs etag-storage (inc etag-value))
  (def global-etag (str etag-value)))

(defn test-global-etag [handler]
  (fn [request]
    (let [etag (get-header request "if-none-match")]
      (if (= global-etag etag)
        { :status 304 :body nil }
        (header (handler request) "ETag" global-etag)))))

; --- add expire

(defn add-expire [handler secs]
  (fn [request]
    (let [response (handler request)
          eternal (get-in request [:params "e"])
          secs (if eternal 10000000 secs)]
      (if (#{200 304 404} (:status response))
        (-> response
            (header "Cache-Control" (str "public, max-age=" secs))
            (header "Expires"
                      (time/format-date (Date. (+ (* 1000 secs) (System/currentTimeMillis))))))
        response))))

; --- cache

(defn cache-response [handler max-size keys]
  (let [cache (atom (cache/lirs-cache-factory {} :s-history-limit max-size :q-history-limit max-size))]
    (fn [request]
      (let [key (select-keys request keys)]
        (if-not (:bot request)
          (if-let [response (cache/lookup @cache key)]
            (do
              #_(debug "HIT " key)
              (swap! cache cache/hit key)
              response)
            (let [response (handler request)]
              #_(debug "MISS " key)
              (when (= 200 (:status response))
                (swap! cache cache/miss key response))
              response))
          (handler request))))))

; --- debug

(def counter (atom 0))

(def ex-log "server-exceptions.txt")

(defn log-exception [e art]
  (let [w (PrintWriter. (FileWriter. ex-log true) true)]
    (.write w (str "---\n" art \newline))
    (.printStackTrace e w)
    (.close w)))

(defn debug-response [handler]
  (fn [request]
    (let [agent (get-in request [:headers "user-agent"])
          lca (.toLowerCase (or agent ""))
          bot (or (.contains lca "bot") (.contains lca "spider"))
          request (if bot (assoc request :bot true) request)
          response (try (handler request) (catch Exception e (log-exception e request) (throw e)))
          mobile (if (.contains lca "mobile") "M" "D")
          bots (if bot "B" "H")
          value (swap! counter inc)]
      (when (or (not bot) (= 404 (:status response)))
        (info value mobile bots
              (or (get-in request [:headers "x-real-ip"]) (:remote-addr request))
              (assoc (dissoc response :body :headers)
                :uri (str (:uri request) (when-let [q (:query-string request)] (str \? q))))))
      response)))

; --- handle empty uri

(defn wrap-empty [handler]
  (fn [request]
    (let [request (if (= (:uri request) "/")
                    (assoc request :uri "/index.html")
                    request)]
      (handler request))))

; --- handle binary responses

(defn test-array [t]
  (let [check (type (t []))]
    (fn [arg] (instance? check arg))))

(def byte-array?
  (test-array byte-array))

(defn wrap-binary [handler]
  (fn [request]
    (let [response (charset (handler request) "UTF-8")
          body (:body response)]
      (if (byte-array? body)
        (assoc response :body (ByteArrayInputStream. ^bytes body))
        response))))

; --- template

(html/deftemplate ns-page-template "content/proto.html" [cx]
  [:title]  (html/content (:title cx))
  [:#precont] (html/content (:cont cx))
  [:head (html/attr= :name "keywords")] (html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:head (html/attr= :rel "canonical")] (html/set-attr "href" (:uri cx))
  [:#nss] (html/content (:nss cx))
  [:#vars] (html/content (html/html (:vars cx)))
  [:#auth] (html/content (:auth cx))
  [:#inverse] (html/content (:inverse cx))
  [:#dgraph] (html/content (:dgraph cx))
  [:#markdown] (html/html-content (:readme cx))
  [:#art] (html/content (:art cx))
  [:#ver] (html/content (:ver cx)))

(html/deftemplate notfound-template "content/notfound.html" [cx]
  [:#random0]  (html/content (:random cx)))

; --- server

(defn str-art [art]
  (str \[ (art 0) " \"" (art 1) "\"]"))

(defn patch-artifact [[n v]]
  (if (.exists (File. (str "resources/content/ns/" n \/ v)))
    [n v]
    (if-let [v1 (first (.list (File. (str "resources/content/ns/" n))))]
      (do
        (println "patched" n v v1)
        [n v1])
      [n v])))

(defn patch-entry [{ a :artifact :as e }]
  (assoc e :artifact (patch-artifact a)))

(defn resource [name]
  (when-let [r (io/resource name)]
    (slurp r :encoding "utf-8")))

(def last-modified-content (time/format-date (Date.)))

(defn url-decode [s] (try (URLDecoder/decode s "utf-8") (catch Exception _ s)))

(def fat-index (map patch-entry (edn/read-string (slurp "resources/fat-index.edn" :encoding "utf-8"))))

(def referred-by (into {} (map (fn [m] [(str (first (:artifact m))) (:refs m)]) fat-index)))
(def referred-by-ver (into {} (map (fn [m] [(str (first (:artifact m))) (:vers m)]) fat-index)))

(defn find-transitive-closure [name visited f]
  (let [visiting (filter (complement visited) (f name))
        visiting+ed (into visited visiting)]
     (into visiting (into #{} (mapcat #(find-transitive-closure % visiting+ed f) visiting)))))

(def transitively-referred-by (into {} (map (fn [k] [k (seq (find-transitive-closure k #{k} referred-by))]) (keys referred-by))))

(def refers-to (reduce (fn [m [k v]] (update m k conj v)) {} (mapcat (fn [[a rs]] (map (fn [x] [x a]) rs)) referred-by)))

(def cljs-projects (apply hash-set "org.clojure/clojurescript" (referred-by "org.clojure/clojurescript")))

(def refers-to-ver (reduce (fn [m [k v]] (update m k conj v)) {}
                           (mapcat (fn [[a rs]] (map (fn [x v] [x v]) rs (referred-by-ver a))) referred-by)))

(def transitively-refers-to (into {} (map (fn [k] [k (seq (find-transitive-closure k #{k} refers-to))]) (keys refers-to))))

(def same-group (group-by :group-id fat-index))

(def clojure-entry {
  :name     " Clojure " :artifact '[org.clojure/clojure "1.7.0-RC1"]
  :category "Core" :main-ns "clojure.core"
  })

(def art->name (into {"org.clojure/clojure" clojure-entry} (map (fn [m] [(str ((:artifact m ["???"]) 0)) m]) fat-index)))

(defn plainify [s]
  (when s
    (-> s
      (str/replace #"\"ç@(.)([^\"ç@]+)ç@([^\"ç@]+)\"" "$2")
      (str/replace #"\"çç@([^\"ç@]+)ç@([^\"ç@]+)\"" "$1")
      (str/replace #"\"ç@([^\"ç@]+)\"" ""))))

(defn color-class [n]
  (cond
    (zero? n)  "1"
    (= n 1)    "2"
    (<= n 4)   "3"
    (<= n 15)  "4"
    (<= n 50)  "5"
    (<= n 300) "6"
    :else      "7"))

(defn dir-entry [e]
  (if e
    (let [art (:artifact e)
          k (count (referred-by (str (art 0))))]
      [(keyword (str "div.dirl.g" (color-class k)))
        {:href-ns (str "/ns/" (art 0) "/latest/" (:main-ns e) ".html")
         :href-doc (str "/doc/" (art 0) "/latest/index.html")
         :href-prj (str "/ns/" (art 0) "/latest/project.clj.html")}
        [:span (:name e)] [:span.q2 " " (art 1)] [:div.q1 (:category e)]])
    [:span]))

; "7|org.clojure/clojure|1.6.0|clojure.core|Clojure|Core"

(defn sanitize [s]
  (if s (-> s
            (str/replace "\n" " ")
            (str/replace "|" " ")
            (hiccup.util/escape-html))
        ""))

(defn dir-entry-compact [e]
  (let [art (:artifact e)
        k (count (referred-by (str (art 0))))]
    (str (color-class k) \| (art 0) \| (art 1) \| (:main-ns e) \| (:name e) \| (sanitize (:category e)))))

(defn proj-id [auth art]
  (let [auth1 (if (= (str auth) (str art)) nil auth)
        art-name (symbol auth1 (str art))
        san (str art-name)]
    san))

(defn is-cljs? [e]
  (cljs-projects (proj-id (:group-id e) (:name e))))

(let [indexlist (map dir-entry-compact
                     (sort-by #(.toLowerCase (:name %)) (filter :main-ns fat-index)))]
  (def dir-index (str/join "|" (conj indexlist (dir-entry-compact clojure-entry))))
  (def dir-index-clojure (str/join "|" (filter #(and (.contains % "org.clojure/") (not (.contains % "google-closure"))) indexlist))))

(let [indexlist (map dir-entry-compact
                     (sort-by #(.toLowerCase (:name %)) (filter is-cljs? (filter :main-ns fat-index))))]
  (def dir-index-cljs (str/join "|" (filter #(and (.contains % "org.clojure/") (not (.contains % "google-closure"))) indexlist))))

(let [recentlist (map dir-entry-compact
                      (sort-by (comp - :date) (filter :main-ns fat-index)))]
  (def dir-index-recency (str/join "|" recentlist))
  (def dir-index-recency20 (str/join "|" (take 20 recentlist))))

(let [recentlist (map dir-entry-compact
                      (sort-by (comp - :date) (filter is-cljs? (filter :main-ns fat-index))))]
  (def dir-index-recency-cljs20 (str/join "|" (take 20 recentlist))))

(let [referlist (map dir-entry-compact
                     (sort-by (comp - count referred-by str first :artifact) (filter :main-ns fat-index)))]
  (def dir-index-refers (str/join "|" referlist))
  (def dir-index-refers50 (str/join "|" (take 250 (filter #(not (.contains % "org.clojure/")) referlist)))))


(let [referlist (map dir-entry-compact
                     (sort-by (comp - count referred-by str first :artifact) (filter is-cljs? (filter :main-ns fat-index))))]
  (def dir-index-refers-cljs (str/join "|" (take 250 (filter #(not (.contains % "org.clojure/")) referlist)))))

(defn text-abstract [s n]
  (if s
    (if (< (.length s) n)
      s
      (let [pos (.indexOf s " " (dec n))]
        (if (= -1 pos)
          s
          (subs s 0 pos))))
    ""))

(defn random-prj [n]
  [:div#random [:h2 "Some other projects..."]
   (map (fn [e]
          (let [art (:artifact e)] [:div.dirl2 [:a {:href (str "/ns/" (art 0) \/ (art 1) \/ (:main-ns e) ".html")}
                                               [:strong (:name e)]] " " [:span.q2o (art 1)] " - " [:em.q1o (text-abstract (:description e "") 70)] " - " [:a.q2o {:href (str "/doc/" (art 0) \/ (art 1) "/index.html") } "Docs"]]))
        (take n (shuffle (filter :main-ns fat-index))))])

(defn plural [count singular plural]
  (str count \space (if (= count 1) singular plural)))

(def date-format (SimpleDateFormat. "MMM d, YYYY"))

(defn dep-item [title content]
  [:div.deps0 [:div.stit title] [:pre.depcode {:onclick "select_all(this);"} content]])

(defn normal-response [response]
  (-> response
      (header "Last-Modified" last-modified-content)
      (content-type "text/html; charset=UTF-8")))

(defn dep-info [art]
  (let [artId (name (art 0))
        groupId (if-let [k (namespace (art 0))] k artId)
        version (art 1)]
    [:div.depsinfo
     (dep-item "LEININGEN" (pr-str art))
     (dep-item "LEIN-TRY" (str "lein try " (art 0) " \"" (art 1) \"))
     (dep-item "MAVEN" (str "<dependency>\n  <groupId>" groupId "</groupId>\n  <artifactId>" artId "</artifactId>\n  <version>" version "</version>\n</dependency>"))
     (dep-item "SBT" (format "\"%s\" %% \"%s\" %% \"%s\"" groupId artId version))
     (dep-item "GRADLE" (format "compile '%s:%s:%s'" groupId artId version))
     #_(dep-item "BUILDR" (format "'%s:%s:jar:%s'" groupId artId version))
     #_(dep-item "IVY" (format "<dependency\n org=\"%s\"\n name=\"%s\"\n rev=\"%s\" />" groupId artId version))
     (dep-item "DOCS PERMALINK" (format "https://crossclj.info/doc/%s/latest/index.html" (str (art 0))))
     [:br] [:div.stit "(Click to select)"]]))

(defn dep-vignette [in0 in1 out0 out1 href]
  (let [rin0 (Math/round (/ (Math/sqrt (* 80 in0)) 2))
        rin1 (Math/round (/ (Math/sqrt (* 80 in1)) 2))
        rout0 (Math/round (/ (Math/sqrt (* 80 out0)) 2))
        rout1 (Math/round (/ (Math/sqrt (* 80 out1)) 2))
        boxin (min 150 (* 2 rin1))
        boxout (min 150 (* 2 rout1))
        height (+ boxin boxout 7)
        middle (+ boxin 6)]
    (list [:div.stit "DEPENDENCY RATIO"]
          [:a {:href href}
           [:svg.vignette {:width 175 :height height :xmlns "http://www.w3.org/2000/svg"}
            [:circle {:cx 87 :cy (- middle rin1) :r rin1 :fill "rgb(100,200,255)"}]
            [:circle {:cx 87 :cy (- middle rin0) :r rin0 :fill "rgb(0,100,235)"}]
            [:circle {:cx 87 :cy (+ middle rout1) :r rout1 :fill "rgb(210,255,160)"}]
            [:circle {:cx 87 :cy (+ middle rout0) :r rout0 :fill "rgb(100,235,90)"}]]])))

(defn dep-vis [name href]
  (dep-vignette (count (refers-to name))
                (count (transitively-refers-to name))
                (count (referred-by name))
                (count (transitively-referred-by name))
                href))

(def version-scheme (GenericVersionScheme.))

(defn common-substring
  ([s1 s2] (common-substring s1 s2 0))
  ([s1 s2 n]
   (if (or (= n (count s1)) (= n (count s2)))
     n
     (if (= (nth s1 n) (nth s2 n))
       (common-substring s1 s2 (inc n))
       n))))

(defn author [dir]
  (when-let [content (resource (str dir "author.html"))]
    (list (html/html-snippet content) [:hr])))

(defn decorate-protocol [s]
  (if (.contains s "~")
    (let [z (.split s "~")
          var-id (nth z 0)
          target (nth z 1)]
      (list var-id " " [:span.pex target]))
    s))

(defn escape-string [content]
  (-> content
      #_(.replace "</" "<\\/")
      (.replace "\\" "\\\\")
      (.replace "\t" "\\t")
      (.replace "\n" "\\n")
      (.replace "\"" "\\\"")
      ))

(defn ns-page [uri auth art ver0 ns request]
  (let [auth1 (if (empty? auth) art auth)
        art-name (symbol auth art)
        san (str art-name)
        art-entry (art->name san)
        ver (get-in art-entry [:artifact 1])
        artifact [art-name ver]
        canonical (str/replace uri (str \/ ver0 \/) (str "/latest/"))
        ns (if (= (str ns) "pippo.html") (symbol (str (:main-ns art-entry) ".html")) ns)
        project? (.endsWith (str ns) "project.clj.html")
        links (volatile! ['([:a {:href "#cont"} "Project" [:br] [:br]])])
        dir (str "content/ns/" (artifact 0) \/ (artifact 1) \/)]
    (when art-entry
      (if (or (= ver0 ver) (= ver0 "latest"))
        (when-let [menu (resource (str dir "menu.html"))]
          (if-let [content (or (resource (str dir ns)) (and (.contains ns "project.clj") "No Leiningen project.clj found in the artifact,\ninferring dependency info only from published POM."))]
            (let [k (str ns)
                  lein-plugin? (and project? (not= art "leiningen") (or (.contains content ":eval-in-leiningen") (and (.contains content ":eval-in") (.contains content ":leiningen"))))
                  kns (subs k 0 (- (count k) 5))
                  cljs? (.endsWith kns ".cljs")
                  kns (if cljs? (subs kns 0 (- (count kns) 5)) kns)
                  menu (list (html/html [:a#so1 {:href "#"} "Source"]
                                        [:a#so2 {:href (if project? (str "/doc/" (artifact 0) \/ (artifact 1) "/index.html") (str "/doc/" (subs uri 4)))} "Docs"] [:br] [:br] [:br]) (html/html-snippet menu))
                  invlist (when project? (let [refs (referred-by san ())
                                               refs2 (refers-to san ())
                                               refs2v (refers-to-ver san ())
                                               outdated (into {}
                                                              (filter
                                                                (fn [[a v]] (> 0 (compare (.parseVersion version-scheme v) (.parseVersion version-scheme ((:artifact (art->name a)) 1)))))
                                                                (map (fn [a v] [a v]) refs2 refs2v)))
                                               outdated (dissoc outdated "org.clojure/clojure" "org.clojure/clojurescript")
                                               refs3 (same-group (str auth1))]
                                           (html/html
                                             (when-not (empty? refs2) [:div.referrers (do (vswap! links conj (list [:a {:href "#uses"} "Uses\u2026"] [:br])) [:a {:id "uses"}]) [:h2 "Uses " (plural (count refs2) "artifact" "artifacts") "; transitively uses " (plural (count (transitively-refers-to san)) "artifact" "artifacts")] [:div.dircolh (map dir-entry (map art->name (sort-by (comp - count referred-by) refs2)))]])
                                             (when lein-plugin? (let [s (set refs2)] [:div.referrers (do (vswap! links conj (list [:a {:href "#leinp"} "Lein Plugins"] [:br])) [:a {:id "leinp"}]) [:h2 "Additional dependencies as Leiningen plugin"] [:div.dircolh (map dir-entry (map art->name (sort-by (comp - count referred-by) (filter #(not (contains? s %)) (map str implicit-deps)))))]]))
                                             (when-not (empty? refs)
                                               (let [gr (group-by second (zipmap refs (map #(.parseVersion version-scheme %) (referred-by-ver san))))]
                                                 [:div.referrers (do (vswap! links conj (list [:a {:href "#used"} "Used by\u2026"] [:br])) [:a {:id "used"}]) [:h2 "Used by " (plural (count refs) "artifact" "artifacts")
                                                                  "; transitively used by " (plural (count (transitively-referred-by san)) "artifact" "artifacts")]
                                                  (let [vss (reverse (sort-by first gr))]
                                                    (list (when (and (> (count vss) 2) (> (count refs) 30))
                                                            [:dir.verrh
                                                             (for [[g vs] (sort-by #(- (count (second %))) vss)] [:span.verc [:a {:href (str "#ver" g)} g] (when (> (count vs) 1) (list " (" (count vs) ")"))])])
                                                          (for [[g vs] vss]
                                                            (list [:a {:id (str "ver" g)}]
                                                                  [:div.verr (plural (count vs) "artifact" "artifacts") " using version " [:span.vertag g]]
                                                                  [:div.dircolh (map dir-entry (map art->name (sort-by (comp - count referred-by) (map first vs))))]))))]))
                                             (when (> (count outdated) 1) [:div.referrers (do (vswap! links conj (list [:a {:href "#outdated"} "Outdated"] [:br])) [:a {:id "outdated"}])
                                                                           [:h2 "‘Outdated’ dependencies"]
                                                                           [:table.da [:tr.da [:th.da "Artifact"] [:th.da "Referenced version"] [:th.da "Most recent version"]]
                                                                            (map (fn [[a v]]
                                                                                   (let [v1 (str v)
                                                                                         v2 (str ((:artifact (art->name a)) 1))
                                                                                         pos (common-substring v1 v2)]
                                                                                     [:tr.da [:td.da [:a {:href (str "/doc/" a "/latest/index.html")} (str a)]] [:td.dav v1] [:td.dav (if (< pos (count v2)) (list (subs v2 0 pos) [:span.hid (subs v2 pos)]) v2)]]))
                                                                                 (sort-by first outdated))]])
                                             (when (> (count refs3) 1) [:div.referrers (do (vswap! links conj (list [:a {:href "#also"} "Group"] [:br])) [:a {:id "also"}]) [:h2 (str "Also in the '" auth1 "' group")] [:div.dircolh (map dir-entry (filter #(not= % art-entry) (sort-by (comp - count referred-by str first :artifact) refs3)))]]))
                                           ))
                  readme (when project? (when-let [readme (resource (str dir "README.md.html"))]
                                          (vswap! links conj (list [:br] [:a {:href "#readme"} "Readme"] [:br]))
                                         (str
                                           "<a id=readme><a/><div class=smallprint>The README below is fetched from the published project artifact. Some links may be broken.</div>"
                                           "<div class=markdown>" readme "</div>")))
                  vars (if project?
                         (let [label san
                               label (if (> (count label) 20) (subs label (inc (.indexOf label "/"))) label)]
                           (list [:span
                                  [:p [:span {:class "glyphicon glyphicon-home"}] " " [:a {:target "_blank" :href (:url art-entry)} label]]
                                  [:p [:span {:class "glyphicon glyphicon-briefcase"}] " " [:a {:target "_blank" :href (str "https://clojars.org/" art-name)} "Clojars"]]
                                  (let [date (:date art-entry)] (when (> date 0) [:p [:span {:class "glyphicon glyphicon-calendar"}] " " (.format date-format date)]))]
                                 [:hr] (seq @links) [:hr] (author dir) (dep-vis san (str uri))  [:hr] (dep-info artifact)))
                         [:span [:span.stit "VARS"] [:br] (map (fn [id] (list [:a {:href (str \# id)} (decorate-protocol (subs id 1))] [:br])) (sort String/CASE_INSENSITIVE_ORDER (distinct (map second (re-seq #"\"ç@([^\"ç@]+)\"" content)))))])]
              (normal-response {:status 200
                                :body   (apply str (ns-page-template
                                                     {:nss         menu
                                                      :cont        (str "$(\"#cont\").html(\"" (escape-string content) "\");")
                                                      :vars        vars
                                                      :title       (str kns " - " (artifact 0) " - CrossClj.info")
                                                      :keywords    (str (artifact 0) ", " (when-not (= kns "project.clj") (str/replace kns \. \space)) ", clojure" (when cljs? "script") ", source code")
                                                      :description (str "Clojure" (when cljs? "Script") " source code of namespace " kns " in artifact " (str-art artifact))
                                                      :uri         (str "https://crossclj.info" canonical)
                                                      :auth        (str (artifact 0))
                                                      :art         kns
                                                      :inverse     invlist
                                                      :readme      readme
                                                      :ver         (artifact 1)}))}))

            (let [dest (str (:main-ns art-entry) ".html")] (when (and project? (not (.startsWith dest "project.clj"))) (redirect (str/replace uri "project.clj.html" dest))))))
        (redirect canonical)))))

(html/deftemplate doc-page-template "content/docu.html" [cx]
  [:title]  (html/content (:title cx))
  [:#hic] (html/append (:cont cx))
  [:head (html/attr= :name "keywords")] (html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:head (html/attr= :rel "canonical")] (html/set-attr "href" (:uri cx))
  [:#nss] (html/content (:nss cx))
  [:#auth] (html/content (:auth cx))
  [:#art] (html/content (:art cx))
  [:#ver] (html/content (:ver cx)))

(defn doc-page [uri auth art ver0 ns]
  (let [ art-name (symbol auth art)
         art-entry (art->name (str art-name))
         ver (get-in art-entry [:artifact 1])
         artifact [art-name ver]
         canonical (str/replace uri (str \/ ver0 \/) (str "/latest/"))
         dir (str "content/doc/" (artifact 0) \/ (artifact 1) \/)
         dir-ns (str "content/ns/" (artifact 0) \/ (artifact 1) \/)
         ns (if (= (str ns) "pippo.html") (symbol "index.html") ns)
         project? (.endsWith (str ns) "index.html")]
    (when art-entry
      (if-not (.endsWith uri "/menu.html")
        (if (or (= ver0 ver) (= ver0 "latest"))
          (when-let [menu (resource (str dir "menu.html"))]
            (when-let [content0 (resource (str dir ns))]
              (let [k (str ns)
                    project-source-target (str "/ns/" (artifact 0) \/ (artifact 1) \/ (if (.exists (File. (str "resources/content/ns/" (artifact 0) \/ (artifact 1) \/ "project.clj.html"))) "project.clj.html" (str (:main-ns art-entry) ".html")))
                    kns (subs k 0 (- (count k) 5))
                    cljs? (.endsWith kns ".cljs")
                    kns (if cljs? (subs kns 0 (- (count kns) 5)) kns)
                    menu (list (html/html [:a#do1 {:href (if project? project-source-target (str "/ns/" (subs uri 5)))} "Source"]
                                          [:a#do2 {:href "#"} "Docs"] [:br] [:br] [:br]) (html/html-snippet menu))
                    readme (when project? (when-let [readme (resource (str dir-ns "README.md.html"))]
                                            [:div#markdown (html/html-snippet (str
                                                                                "<a id=xread><div class=smallprint>The README below is fetched from the published project artifact. Some relative links may be broken.</div>"
                                                                                "<div class=markdown>" readme "</div>"))]))
                    content (if project?
                              (let [label (str art-name)
                                    label (if (> (count label) 20) (subs label (inc (.indexOf label "/"))) label)]
                                (html/html [:div.c41 [:span
                                                      [:p [:span {:class "glyphicon glyphicon-home"}] " " [:a {:target "_blank" :href (:url art-entry)} label]]
                                                      [:p [:span {:class "glyphicon glyphicon-briefcase"}] " " [:a {:target "_blank" :href (str "https://clojars.org/" art-name)} "Clojars"]]
                                                      (let [date (:date art-entry)] (when (> date 0) [:p [:span {:class "glyphicon glyphicon-calendar"}] " " (.format date-format date)]))]
                                            [:hr] (author dir-ns) (when readme [:a {:href "#xread"} "Readme"])]
                                           [:div.c42 (html/html-snippet (str/replace content0 "@@@desc@@@" (:description art-entry ""))) readme]))
                              (html/html (html/html-snippet content0)))]
                (normal-response {:status 200
                         :body   (apply str (doc-page-template
                                              {:nss         menu
                                               :cont        content
                                               :title       (str kns " - " (artifact 0) " - CrossClj.info")
                                               :keywords    (str (artifact 0) ", " (when-not (= "project.clj" kns) (str/replace kns \. \space)) ", clojure" (when cljs? "script"))
                                               :description (str "Clojure" (when cljs? "Script") " documentation for namespace " kns " in artifact " (str-art artifact))
                                               :uri         (str "https://crossclj.info" canonical)
                                               :auth        (html/html [:a {:itemprop "url" :href (str "/doc/" (artifact 0) "/latest/index.html")} [:span.k0 {:itemprop "title"} (str (artifact 0))]])
                                               :art         (html/html [:a {:itemprop "url" :href canonical} [:span.k1 {:itemprop "title"} kns]])
                                               :ver         (artifact 1)}))}))))
          (redirect canonical))
        (redirect (str/replace canonical "/menu.html" "/index.html"))))))

(html/deftemplate fun-page-template "content/fun.html" [cx]
  [:title]  (html/content (:title cx))
  [:#contx] (html/content (:cont cx))
  [:#precont] (html/content (:source cx))
  [:#sim] (html/content (:sim cx))
  [:#simd] (html/content (:simd cx))
  [:#arglist] (html/content (:arglist cx))
  [:#dox] (html/content (:docs cx))
  [:#cont] (html/set-attr "class" (:sline cx))
  [:#slp] (html/set-attr "style" (:slp cx))
  [:#reshd] (html/content (:reshd cx))
  [:#grimo] (html/content (:grimo cx))
  [:head (html/attr= :name "keywords")](html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "summary")] (html/set-attr "content" (:summary cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:#projs] (html/content (:projs cx))
  [:#ns] (html/content (:ns cx))
  [:#random0]  (html/content (:random cx))
  [:#ns] (html/set-attr "href" (:nsa cx))
  [:#fun] (html/content (:fun cx))
  [:#sclink] (html/content (:sclink cx))
  [:#fun] (html/set-attr "href" (:funa cx)))

(defn munge-var [s]
  (-> s
      (str/replace "?" "_QMARK_")
      (str/replace "." "_DOT_")
      (str/replace "/" "_SLASH_")
      (str/replace #"^_*" "")
      (str/replace #"_*$" "")))

(defn to-q [s]
    (.trim (str/replace s #"[^a-zA-Z0-9]+" " ")))

(defn fun-page [uri ns fun request]
  (let [fun2 (if (.endsWith fun ".html") (subs fun 0 (- (count fun) 5)) fun)
        cljs? (.endsWith ns ".cljs")
        ns-display (if cljs? (subs ns 0 (- (count ns) 5)) ns)
        results (map #(% doc-id) (do-search (symbol ns fun2) search-fun [doc-id]))
        docs (first (do-search (symbol ns fun2) find-symbol [doc-text doc-args doc-art doc-vtype doc-source doc-source-start]))
        dargs (when docs (docs doc-args))
        art (when docs (str/split (docs doc-art) #"\|"))
        proj->res (reduce (fn [m v]
                            (when v (let [value (str/split v #",")
                                          key [(value 0) (value 1)]]
                                      (update m key conj value)))) {} results)
        names->res (into {} (map (fn [[[k1 _] v]] [k1 v]) proj->res))
        restrict (get-in request [:params "r"])
        all? (= (get-in request [:params "all"]) "y")
        max-per-proj (if (and (not restrict) (> (count names->res) 5)) 5 10000)
        names->res (if restrict {restrict (names->res restrict)} names->res)
        menu [:div.ns
              (when-not (empty? names->res) [:div.stit "PROJECTS"])
              (map #(list [:a {:href (str "#" %)} (min-namespace-name %)] [:br]) (sort (keys names->res)))]
        examples0 (sort-by (fn [[k _]]  (- (count (referred-by k ())))) names->res)
        examples (if all? examples0 (take 50 examples0))
        content0 (map (fn [[k v]]
                             (let [m (art->name k)]
                               [:div.bfl [:a {:id k}]
                                         [:div (plural (count v) "usage" "usages") " in "]
                                         (dir-entry m)
                                       (when (> (count v) max-per-proj) (list [:br] [:a.all {:href (str uri "?r=" k)} "Show all"]))
                                       [:div (map (fn [[art ver ns fun]]
                                                      (let [ns-display (if (.endsWith ns ".cljs") (subs ns 0 (- (count ns) 5)) ns)]
                                                        [:br] [:div [:a {:href (str "/ns/" art \/ ver \/ ns ".html#_" ((fnil hu/url-encode "bah") fun))} (str (min-namespace-name ns-display 16)) \/ [:strong (decorate-protocol fun) ]]])) (take max-per-proj v))]]))
                      examples)
        els1 (take-nth 2 content0)
        els2 (take-nth 2 (next content0))
        txt (to-q fun2)
        content [:span [:div.dircol els1] [:div.dircol els2]]]
    (normal-response {:status 200
                      :body   (apply str (fun-page-template
                                           {:projs       (html/html menu)
                                            :cont        (html/html content)
                                            :source      (when docs (let [d (docs doc-source)] (str "$(\"#cont\").html(\"" (escape-string d) "\");")))
                                            :docs        (when docs (docs doc-text))
                                            :summary     (when (and docs (docs doc-text)) (-> (docs doc-text) (str/replace "\n" " ") (hu/escape-html)))
                                            :sline       (str "brush:clojure;first-line:" (if-let [line (and docs (docs doc-source-start))] (+ 1 (read-string line)) "1"))
                                            :random      (html/html (random-prj 7))
                                            :nsa         (if art (str "/ns/" (art 0) \/ (art 1) \/ ns ".html") "#")
                                            :funa        (if art (str "/ns/" (art 0) \/ (art 1) \/ ns ".html#_" (hu/url-encode fun2)) "#")
                                            :sclink      (if art (html/html [:a.bfl {:href (str "/ns/" (art 0) \/ (art 1) \/ ns ".html#_" (hu/url-encode fun2))} ns-display (when cljs? " (cljs)") " (" (art 1) ")"]))
                                            :arglist     (when dargs
                                                           (html/html [:ul (map
                                                                             (fn [z] (when (and z (< 1 (count z))) [:li [:span.funn (str \( fun2 \space)] (subs z 1 (- (count z) 1)) [:span.funn \)]])) (str/split dargs #","))]))
                                            :title       (str ns-display \/ fun2 (when cljs? " (cljs)") " - CrossClj.info")
                                            :sim         (html/html [:a.bflink {:href (str "/clojure/" (hu/url-encode fun2) ".html")} fun2])
                                            :simd        (html/html [:a.bflink1 {:href (str "/docs.html?q=" (hu/url-encode txt))} txt])
                                            :slp         (if (empty? txt) "display:none" "")
                                            :keywords    (str ns-display ", " fun2 ", clojure")
                                            :description (str "Source code and usage of var " fun2 " in namespace " ns-display " of artifact " art)
                                            :grimo
                                                         (when (#{"clojure.core" "clojure.core.async" "clojure.core.logic"} ns)
                                                           (html/html "Browse community-provided documentation and examples on "
                                                                      [:a {:target "_blank" :rel "nofollow" :href (str "http://clojuredocs.org/" ns \/ (hu/escape-html fun2))} "ClojureDocs"]
                                                                      (if (= ns "clojure.core")
                                                                        (list ", "
                                                                              [:a {:target "_blank" :rel "nofollow" :href (str "http://conj.io/store/org.clojure/latest/clj/" ns \/ (munge-var fun2) \/)} "conj.io"] ", "
                                                                              [:a {:target "_blank" :rel "nofollow" :href (str "http://getclojure.com/search?q=" (hu/escape-html fun2) "&num=0")} "GetClojure"]))))
                                            :reshd       (if (empty? results)
                                                           (str "No usages found for " ns-display \/ fun2 (when cljs? " (cljs)"))
                                                           (html/html [:span [:strong ns-display \/ fun2] (when cljs? " (cljs)") " found in " (plural (count results) "def" "defs") ", across " (plural (count proj->res) "project" "projects") ". "
                                                                       (when (and (not all?) (> (count examples0) 50)) [:a {:href "?all=y"} "Show all"])]))
                                            :uri         (str "http://crossclj.info" uri)
                                            :ns          ns-display
                                            :fun         fun2}))})))

(defn split-name [r]
  (let [art (str/split (r doc-art) #"\|")
        art (if (= 2 (count art)) art ["" ""])
        name (r doc-n)]
    {:type (keyword (r doc-type))  :name name :art art :text (r doc-text)}))

(defn var-item [v]
  (let [name (:name v)
        f (symbol name)
        ns (namespace f)
        cljs? (.endsWith ns ".cljs")
        ns-display (if cljs? (subs ns 0 (- (count ns) 5)) ns)
        art (:art v)
        text (:text v)
        link (str (art 0) \/ (art 1) \/ ns ".html#_" ((fnil hu/url-encode "bah") (clojure.core/name f)))
        src-link (str "/ns/" link)
        doc-link (str "/doc/" link)
        ]
    [:li [:span.pjn (art 0)] " "
         [:a {:href src-link} [:span.kkl ns-display] \/ (clojure.core/name f)]
         (when cljs? " (cljs)") " "
         [:a.bfu {:href src-link} "Src"] " "
         [:a.bfu {:href doc-link} "Docs"] " "
         [:a.bfu {:href (str "/fun/" ns \/ (hu/url-encode (clojure.core/name f)) ".html")} "Usages"]
         (when text (list [:br] [:pre.docs text]))
         ]))

(defn proj-item [v]
  (let [ art (:art v)
         e (art->name (art 0))] [:li [:a {:href (str "/ns/" (art 0) \/ (art 1) \/ (:main-ns e) ".html")} (:name e)]]))

(defn ns-item [v]
  (let [name (:name v)
        art (:art v)]
    [:li [:a {:href (str "/ns/" (art 0) \/ (art 1) \/ name ".html")} name]]))

(defn search [name]
  (let [results (map split-name (distinct (do-search (str/lower-case name) search-symbol [doc-n doc-art doc-type doc-text])))]
    results))

(defn correct-search [name results]
  (if (or (.contains name "clojure") (.contains name "core"))
    (conj results {:type :ns :name "clojure.core"
             :art ['org.clojure/clojure "1.8.0"]
             :text "Clojure core language"})
    results))

(defn parse-int [s]
  (if s (try (Integer. s) (catch Exception _ 10)) 100))

(defn search-api [params]
  (let [{query "query" limit "limit" cljs "cljs"} params
        limit (parse-int limit)
        lang-filter (if cljs #(cljs-projects %) identity)]
    (if query
      (let [results (->> (search query)
                         (filter #(not= (:type %) :prj))
                         (filter #(lang-filter (str (first (% :art)))))
                         (sort-by (comp - count referred-by str first :art))
                         (correct-search query)
                         (map (fn [m] (let [name (str/split (:name m) #"/")
                                            ns (first name)
                                            name (second name)]
                                        (-> m
                                            (assoc :ns ns :name (if name name ""))
                                            (update :type (fn [s] (if (= s :ns) 1 2)))
                                            (update :text (fn [s] (if s (let [s2 (str/replace s #"\s{2,15}" " ")] (if (> (count s2) 150) (str (str/trim (subs s2 0 150)) "\u2026") s2)) "")))
                                            (update :art (fn [[a _]] (str a)))
                                            )))))]
        (take limit results))
      [])))

(html/deftemplate names-page-template "content/names.html" [cx]
  [:title]  (html/content (:title cx))
  [:#vars] (html/content (:vars cx))
  [:#nss] (html/content (:nss cx))
  [:#projs] (html/content (:projs cx))
  [:#random0]  (html/content (:random cx))
  [:#simd] (html/content (:simd cx))
  [:head (html/attr= :name "keywords")] (html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:head (html/attr= :rel "canonical")] (html/set-attr "href" (:uri cx))
  [:#name] (html/content (:name cx)))

(defn names-page [name]
  (let [results (search name)
        content (group-by :type results)]
    (normal-response { :status 200
          :body   (apply str (names-page-template
                    {:cont     (html/html content)
                     :title   (str name " - CrossClj.info")
                     :keywords (str name ", clojure")
                     :description (str "Catalog of all vars with a name similar to " name)
                     :random   (html/html (random-prj 7))
                     :uri  (str "http://crossclj.info/clojure/" (hu/url-encode name) ".html")
                     :simd (let [txt (to-q name)]
                             (html/html [:a.bflink1 {:href (str "/docs.html?q=" (hu/url-encode txt))} txt]))
                     :vars
                        (let [l (:var content)] (if (= 0 (count l)) (html/html [:li "No vars found."])
                                                                    (html/html [:ol (map var-item (sort-by (comp - count referred-by str first :art) l))])))
                     :nss
                         (let [l (:ns content)] (if (= 0 (count l)) (html/html [:li "No namespaces found."]) (html/html [:ol (map ns-item l)])))
                     :projs
                         (let [l (:prj content)] (if (= 0 (count l)) (html/html [:li "No projects found."]) (html/html [:ol (map proj-item l)])))

                     :name     (str name)}))})))

(def last-modified-index (Date. (.lastModified (File. "resources/fat-index.edn"))))

(def last-modified-date (.format date-format last-modified-index))

(html/deftemplate home-page-template "content/home.html" [cx]
  [:#data] (html/content (:data cx))
  [:.count]  (html/content (str (count fat-index)))
  [:#blurb] (html/content (:blurb cx)))

(html/deftemplate homecljs-page-template "content/homecljs.html" [cx]
  [:#data] (html/content (:data cx))
  [:.count]  (html/content (str (count fat-index)))
  [:#blurb] (html/content (:blurb cx)))

(html/deftemplate docusearch-page-template "content/docusearch.html" [cx]
  [:.count]  (html/content (str (count fat-index)))
  [:head (html/attr= :name "keywords")] (html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:title]  (html/content (:title cx))
  [:#blurb] (html/content (:blurb cx))
  [:#oink] (html/set-attr "value" (:value cx))
  [:#searchresults] (html/content (:results cx)))

(html/deftemplate docusearchcljs-page-template "content/docusearchcljs.html" [cx]
  [:.count]  (html/content (str (count fat-index)))
  [:head (html/attr= :name "keywords")] (html/set-attr "content" (:keywords cx))
  [:head (html/attr= :name "description")] (html/set-attr "content" (:description cx))
  [:title]  (html/content (:title cx))
  [:#blurb] (html/content (:blurb cx))
  [:#oink] (html/set-attr "value" (:value cx))
  [:#searchresults] (html/content (:results cx)))

(defn home-page-handler [_]
  (normal-response {:status 200
             :body   (apply str (home-page-template
                                  {:data  (format "var stp1=\"%s\",stp2=\"%s\",stpc=\"%s\";"
                                             dir-index-recency20 dir-index-refers50 dir-index-clojure)

                                   :blurb (html/html [:div "Project count:" [:br] (count fat-index) [:br] [:br]
                                                      "Last update" [:br] last-modified-date])}))}))

(defn homecljs-page-handler [_]
  (normal-response {:status 200
             :body   (apply str (homecljs-page-template
                                  {:data  (format "var stp1=\"%s\",stp2=\"%s\",stpc=\"%s\";"
                                             dir-index-recency-cljs20 dir-index-refers-cljs dir-index-cljs)

                                   :blurb (html/html [:div "Project count (cljs):" [:br] (count cljs-projects) [:br] [:br]
                                                      "Last update" [:br] last-modified-date])}))}))

(defn- xml-str
 "Like clojure.core/str but escapes < > and &."
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))

(defn find-docs [q cljs]                                         ;; todo
  (try
    (let [qp (QueryParser. Version/LUCENE_4_10_1 doc-text analyzer)
          q (str/replace q ":" "\\:")
          lq (.parse qp q)
          nq (.rewrite index-searcher lq)
          terms0 (LinkedHashSet.)
          _ (.extractTerms nq terms0)
          terms (take 50 terms0)
          pat (re-pattern (if (empty? terms) "!/$&" (str "(?i:" (str/join "|" (map (fn [t] (str "(?:\\Q" (.text t) "\\E)")) terms)) ")")))
          hits (.search index-searcher lq 5000)
          n0 (.totalHits hits)
          n (min n0 (if cljs 2000 500))
          results (map (fn [d] (reduce #(assoc %1 %2 (.get d %2)) {} [doc-text doc-args doc-art doc-vtype doc-n])) (found-docs hits 0 n))
          results (if cljs (filter #(cljs-projects (str (first (str/split (% doc-art) #"\|")))) results) results)
          n0 (if cljs (count results) n0)
          prc (count (distinct (map (fn [d] (d doc-art)) results)))]
          results (if cljs (take 500 results) results)
      #_(println terms0)
      [:div.srac [:div.rcount (str "Found " (plural n0 "result" "results") " in " (plural prc "project." "projects. ") (when (> n0 n) (str "Show the first " n ". "))) [:span.share]]
                               (map (fn [d] (let [a (d doc-art)
                                                  art (str/split a #"\|")
                                                  entry (dir-entry (art->name (art 0)))
                                                  name0 (d doc-n)
                                                  name1 (if (.contains name0 ".cljs/") (str (.replace name0 ".cljs/" "/") " (cljs)") name0)
                                                  sname (symbol name0)
                                                  ns (hu/url-encode (namespace sname))
                                                  vid (URLEncoder/encode (name sname))
                                                  usages-url (str "/fun/" ns \/ vid ".html")
                                                  ns-href (str "/ns/" (art 0) \/ (art 1) \/ ns ".html")
                                                  doc-href (str "/doc/" (art 0) \/ (art 1) \/ ns ".html#_" vid)
                                                  ]
                                              (list [:div [:div.hdir entry " " [:span.sran (el/link-to doc-href name1)
                                                                                [:span.tools (el/link-to (str ns-href "#_" vid) "Source")
                                                                                 (el/link-to usages-url "Doc + Usages")]]
                                                           ] [:pre.sras (html/html-snippet (str/replace (xml-str (d doc-text)) pat
                                                                                                        "<span class=hid>$0</span>"))]] [:hr]))) results)])
    (catch Throwable e
      (println e)
      [:div.srac [:div.rcount "Syntax error."]])))

(defn docusearch-page-handler [{params :params}]
  (let [q (params "q")
        results (when q (find-docs q false))]
    (normal-response {:status 200
                      :body   (apply str (docusearch-page-template
                                           {:results (html/html results)
                                            :value q
                                            :keywords (str q " clojure docs")
                                            :description  (str "Clojure/ClojureScript fns dealing with '" q \')
                                            :title  (str (when q (str q " - ")) "Clojure documentation search - CrossClj - cross-referencing the clojure ecosystem")

                                            :blurb (html/html [:div "Project count:" [:br] (count fat-index) [:br] [:br]
                                                               "Last update" [:br] last-modified-date])}))})))

(defn docusearchcljs-page-handler [{params :params}]
  (let [q (params "q")
        results (when q (find-docs q true))]
    (normal-response {:status 200
                      :body   (apply str (docusearchcljs-page-template
                                           {:results (html/html results)
                                            :value q
                                            :keywords (str q " clojurescript docs")
                                            :description  (str "Clojure/ClojureScript fns dealing with '" q \')
                                            :title  (str (when q (str q " - ")) "Clojurescript documentation search - CrossClj - cross-referencing the clojure ecosystem")

                                            :blurb (html/html [:div "Project count (cljs):" [:br] (count cljs-projects) [:br] [:br]
                                                               "Last update" [:br] last-modified-date])}))})))

(html/deftemplate catalog-page-template "content/catalog.html" [cx]
  [:#index] (html/content (:index cx))
  [:#index2] (html/content (:index cx))
  [:#catalog] (html/content (:data cx)))

(def catalog (group-by #(Character/toUpperCase (first (:name %))) (filter :main-ns fat-index)))

(defn catalog-page-handler [request]
  (let [index (Character/toUpperCase (first (str (get-in request [:params "index"] "A") "*")))]
    (normal-response {:status 200
                      :body   (apply str (catalog-page-template
                                           {:data (html/html [:table (map (fn [e] [:tr [:td.catcell (let [n (str (:name e) " (" ((:artifact e) 1) ")")] (if (< (count n) 32) n (str (subs n 0 31) \u2026)))]
                                                                                   [:td.catcell " " [:a {:href (str "/ns/" ((:artifact e) 0) "/latest/" (:main-ns e) ".html")} "src"]]
                                                                                   [:td.catcell " " [:a {:href (str "/doc/" ((:artifact e) 0) "/latest/index.html")} "docs"]]
                                                                                   [:td.catcell " " [:a {:href (str "/ns/" ((:artifact e) 0) "/latest/project.clj.html")} "project"] " "]
                                                                                   [:td.catcell.ccmx (let [n (:description e)] (if (< (count n) 151) n (str (subs n 0 150) \u2026))) ]
                                                                                   ])
                                                                          (if-let [c (catalog index)] c (apply concat (vals catalog))))])
                                            :index (html/html [:div (map (fn [x] (if (= x index) [:span.indexnav (str x)] [:a.indexnav {:href (str "/catalog.html?index=" x)} (str x)])) (conj (keys catalog) \*))])

                                            }))})))

(html/deftemplate all-page-template "content/all.html" [cx]
  [:#data] (html/content (:data cx))
  [:#sort] (html/content (:sort cx))
  [:#blurb] (html/content (:blurb cx)))

(defn all-page-handler [request]
  (let [ord (get-in request [:params "sort"])]
    (normal-response {:status 200
             :body   (apply str (all-page-template
                                  {:data  (str "var stp=\"" (case ord
                                                         "d" dir-index-recency
                                                         "r" dir-index-refers
                                                         dir-index) "\";")
                                   :sort (html/html [:span.sorth "Sort by "
                                                     [:code
                                                     [:a {:href "?" :class (if-not ord "sel" "unsel")} "Name"] " "
                                                     [:a {:href "?sort=d" :class (if (= ord "d") "sel" "unsel")} "Date"] " "
                                                     [:a {:href "?sort=r" :class (if (= ord "r") "sel" "unsel")} "Deps"] " "]])
                                   :blurb (html/html [:div "Project count:" [:br] (count fat-index) [:br] [:br]
                                                      "Last update" [:br] last-modified-date])}))})))

(def notfound {:status 404})

(defn notfound-page-handler [{uri :uri}]
  (if (.endsWith uri ".html")
    (normal-response {:status 200
             :body   (apply str (notfound-template { :random (html/html (random-prj 9)) }))})
    notfound))

(defn ns-page-handler [request]
  (let [uri (:uri request)
        parts (map url-decode (str/split uri #"/"))]
    (if (.endsWith uri "/undefined")
      (redirect "/index.html")
      (case (count parts)
        5 (let [[_ _ art ver ns] parts]
            (or (ns-page uri nil art ver ns request) (notfound-page-handler request)))
        6 (let [[_ _ auth art ver ns] parts]
            (or (ns-page uri auth art ver ns request) (notfound-page-handler request)))
        (notfound-page-handler request)))))

(defn doc-page-handler [request]
  (let [uri (:uri request)]
    (if (.endsWith uri "/undefined")
        (redirect "/index.html")
        (let [uri (if (.endsWith uri ".html") uri (str uri (if (.endsWith uri "/") "index.html" "/index.html")))
              parts (map url-decode (str/split uri #"/"))]
           (case (count parts)
             5 (let [[_ _ art ver ns] parts]
                 (or (doc-page uri nil art ver ns) (notfound-page-handler request)))
             6 (let [[_ _ auth art ver ns] parts]
                 (or (doc-page uri auth art ver ns) (notfound-page-handler request)))
             (notfound-page-handler request))))))

(defn fun-page-handler [request]
  (let [uri (:uri request)
        parts (map url-decode (str/split uri #"/"))]
    (case (count parts)
      4 (let [[_ _ ns fun] parts]
          (or (fun-page uri ns fun request) (notfound-page-handler request)))
      (notfound-page-handler request))))

(defn usage-page-handler [request]
  (let [uri (:uri request)
        name (url-decode (last (str/split uri #"/")))
        name (if (.endsWith name ".html") (subs name 0 (- (count name) 5)) name)]
    (names-page name)))

(defn name-page-handler [{params :params}]
    (when-let [name (params "q")]
       (names-page name)))

(defn api-handler [request]
  (-> {:status 200
         :body   (json/generate-string
                   (case (get-in request [:params :command])
                     "prefetch-home" dir-index-refers
                     "prefetch-home-cljs" dir-index-refers-cljs
                     "search" (search-api (:params request))))
         }
        (content-type "application/json")))

(defn reload-server []
  (println "Reloading server...")
  (load "/crossclj/index")
  (load "/crossclj/generate")
  (load "/crossclj/server"))

(defn reload-server-handler [_request]
  (let [timer (Timer. true)
        task (proxy [TimerTask] [] (run [] (.start (Thread. #(reload-server)))))]
    (.schedule timer task 1000)
    (normal-response {:status 202 :body "Reloading issued"})))

(def all-routes
  (routes
          (GET "/ns/*" [] ns-page-handler)
          (GET "/doc/*" [] doc-page-handler)
          (GET "/fun/*" [] fun-page-handler)
          (GET "/clojure/*" [] usage-page-handler)
          (GET "/api/v1/:command" [] api-handler)
          (GET "/index.html" [] home-page-handler)
          (GET "/docs.html" [] docusearch-page-handler)
          (GET "/docs" [] docusearch-page-handler)
          (GET "/docsjs.html" [] docusearchcljs-page-handler)
          (GET "/docsjs" [] docusearchcljs-page-handler)
          (GET "/cljs.html" [] homecljs-page-handler)
          (GET "/cljs" [] homecljs-page-handler)
          (GET "/search" [] name-page-handler)
          (GET "/all.html" [] all-page-handler)
          (GET "/catalog.html" [] catalog-page-handler)
          (GET "/reloadXXX" [] reload-server-handler)
          (GET "/*" [] (-> notfound-page-handler
                           (wrap-resource "/site")))))
(def app
    (-> all-routes
        (wrap-params)
        (wrap-gzip)
        #_(cache-response 1000 [:uri :query-string :gzip])
        (wrap-not-modified)
        (test-global-etag)
        (wrap-content-type)
        (wrap-empty)
        (test-gzip)
        (add-expire 9600)
        (wrap-binary)
        (debug-response)))

(def ^:redef ^Server server)

(when (bound? #'server)
  (println "Shutting down old instance...")
  (.stop server))

(def server
  (do (println "Starting server...")
      (jetty/run-jetty
        app
        {:port   8080
         :join?  false
         :daemon true})))



(defn -main [& args]
  (println "started."))
