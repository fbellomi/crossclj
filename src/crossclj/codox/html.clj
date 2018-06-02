(ns crossclj.codox.html
  "Documentation writer that outputs HTML."
  (:use [hiccup core page element util])
  (:import [java.net URLEncoder]
           [org.pegdown PegDownProcessor Extensions LinkRenderer LinkRenderer$Rendering]
           [org.pegdown.ast WikiLinkNode])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [crossclj.codox.utils :as util]
            [crossclj.index :as index]))

(defn- var-id [var]
  (str "_" (-> var name URLEncoder/encode)))

(defmulti format-doc
  (fn [_project _ns var] (:doc/format var))
  :default :plaintext)

(defn add-highlight [s]
  (when s (-> s
              (str/replace #"(^|\p{Space})(:[a-zA-Z\-0-9/<>\*\?]+)(\p{Space}|$)" "$1<span class=z>$2</span>$3")
              (str/replace #"`([^\s]+)`" "<b>$1</b>")
              )))

(defn- documented? [metadata]
  (pos? (count (:doc metadata ""))))

(defmethod format-doc :plaintext [_ _ metadata]
  (when (documented? metadata)
    [:pre (add-highlight (h (:doc metadata)))]))

(def ^:private pegdown
  (PegDownProcessor.
   (bit-or Extensions/AUTOLINKS
           Extensions/QUOTES
           Extensions/SMARTS
           Extensions/STRIKETHROUGH
           Extensions/TABLES
           Extensions/FENCED_CODE_BLOCKS
           Extensions/WIKILINKS
           Extensions/DEFINITIONS
           Extensions/ABBREVIATIONS)
   2000))

(defn- find-wikilink [project ns text]
  (let [ns-strs (map (comp str :name) (:namespaces project))]
    (if (contains? (set ns-strs) text)
      (str text ".html")
      (if-let [var (util/search-vars (:namespaces project) text (:name ns))]
        (str (namespace var) ".html#" (var-id var))))))

(defn- link-renderer [project ns]
  (proxy [LinkRenderer] []
    (render
      ([node]
         (if (instance? WikiLinkNode node)
           (let [text (.getText node)]
             (LinkRenderer$Rendering. (find-wikilink project ns text) text))
           (proxy-super render node)))
      ([node text]
         (proxy-super render node text))
      ([node url title text]
         (proxy-super render node url title text)))))

(defmethod format-doc :markdown [project ns metadata]
  [:div.md2
   (if-let [doc (:doc metadata)]
     (.markdownToHtml pegdown doc (link-renderer project ns)))])

(defn- ns-filename [_cljs? namespace]
  (str (:name namespace) ".html"))

(defn- ns-filepath [cljs? output-dir namespace]
  (str output-dir "/" (ns-filename cljs? namespace)))

(defn- var-uri [cljs? namespace var]
  (str (ns-filename cljs? namespace) "#" (var-id (:name var))))

(defn- get-mapping-fn [mappings path]
  (some (fn [[re f]] (if (re-find re path) f)) mappings))

(defn- split-ns [namespace]
  (str/split (str namespace) #"\."))

(defn- namespace-parts [namespace]
  (->> (split-ns namespace)
       (reductions #(str %1 "." %2))
       (map symbol)))

(defn- index-by [f m]
  (into {} (map (juxt f identity) m)))

(defn- sorted-public-vars [namespace]
  (sort-by (comp str/lower-case :name) (:publics namespace)))

(defn- var-class [var]
  (if (:public var) (when (documented? var) {:class "docd"}) {:class "pri"}))

(defn- vars-menu [cljs? namespace]
  [:span#vars
   [:span.stit "VARS"] [:br]
    (for [var (sorted-public-vars namespace)]
      (list
       [:a (merge {:href (var-uri cljs? namespace var)} (var-class var)) (h (:name var)) ]
       [:br]
       (when (:members var)
         [:div.trin (for [mem (:members var)]
                      (list (link-to (var-uri cljs? namespace mem) (h (:name mem))) [:br]))])))
    [:hr]])

(defn alpha-g [var]
  (let [n (str (:name var))]
    (Character/toUpperCase (or (some #(when (Character/isAlphabetic (int %)) %) n) \-))))

(defn- index-page [project artifact]
  (html [:h2 "Index of all namespaces"]
         [:br]
        [:div.gdocs
         (link-to (str "/ns/" (artifact 0) \/ (artifact 1) "/project.clj.html") "\u00AB Project + dependencies") [:br] [:br]
         [:p.sum "@@@desc@@@"]
         (doall (for [namespace (sort-by :name (:namespaces project))]
                  (do #_(println (:name namespace))
                      (let [nn (str (:name namespace))
                            cljs? (:cljs namespace)
                            ns-href (str "/ns/" (artifact 0) \/ (artifact 1) \/ nn ".html")]
                        [:div.namespace
                         (let [dlink (ns-filename cljs? namespace)]
                           [:div
                            [:span.vname (link-to dlink (h (if cljs? (subs nn 0 (- (count nn) 5)) nn))) (when cljs? " \u2014 cljs")]
                            [:span.tools (link-to dlink "Docs")]
                            [:span.tools (link-to (str ns-href) "Source")]])
                         [:div.doc (format-doc project nil (update-in namespace [:doc] util/summary))]
                         [:div.index
                          (let [vars (sorted-public-vars namespace)]
                            (if (> (count vars) 30)
                              (let [vz (sort-by first (group-by alpha-g vars))]
                                [:p.stit "VARS" (map (fn [[g vars]] [:p.line18 [:span.varh (str g)]  " "
                                                                     (for [var vars]
                                                                          [:span [:a (merge {:href (var-uri cljs? namespace var)}
                                                                                            (var-class var)) (h (:name var))] " "])]) vz)])
                              (when-not (empty? vars)
                                (list [:p.stit "VARS"]
                                      [:p.line18 (for [var vars]
                                         [:span [:a (merge {:href (var-uri cljs? namespace var)}
                                                           (var-class var)) (h (:name var))] " "])]))))]]))))]))

(defn- var-usage [var]
  (for [arglist (:arglists var)]
    (list* (:name var) (if (sequential? arglist) arglist ["???"]))))

(defn- var-docs [project namespace ns-href cljs? var]
  (let [vid (var-id (:name var))
        vs (symbol (str (:name namespace)) (str (:name var)))
        vtype (or (:forms var) (@index/type-env vs))
        usages-url (str "/fun/" (url-encode (:name namespace))  \/ (subs vid 1) ".html")]
    (list
      [:div.var [:a {:id (h vid)}]
           [:div.vhead [:span.vname (link-to usages-url (h (:name var)))]
            (if-let [added (:added var)]
              [:span.since added])
            [:span.tools (link-to (str ns-href "#" vid) "Source")
             (link-to usages-url "Usages")]]
           [:div
            (if-not (= (:type var) :var)
              [:span.attr (name (:type var))])
            (if-not (:public var)
              [:span.attr "Private"])
            (if (:dynamic var)
              [:span.attr "Dynamic"])
            (when (:deprecated var)
              [:span.attr "Deprecated"])]
           [:div.usage
            (for [form (var-usage var)]
              [:code [:span.funn \( (h (first form))] (map #(str \space %) (rest form)) [:span.funn \)]])]
           (when vtype
             (let [vtype (if (vector? vtype) vtype (list vtype))]
               [:div.vtydoc (map (fn [x] [:code (index/clean-type x)]) vtype)]))
           [:div.doc (format-doc project namespace var)]
           (when (= (:type var) :protocol) (when-let [members (seq (:members var))]
                                             [:div.protmemb
                                              (let [project (dissoc project :src-dir-uri)]
                                                (map #(var-docs project namespace ns-href cljs? %) members))]))])
    ))

(defn namespace-page [cljs? project namespace artifact]
  (html [:div.c41
         (vars-menu cljs? namespace)]
        [:div.c42.gdocs
         (link-to "index.html" "\u00AB Index of all namespaces of this project")
         [:br] [:br]
         (when-let [dd (format-doc project nil namespace)]
           [:div.sum dd (when-let [author (:author namespace)] [:p.author "\u2014 " author])])
         (let [ns-href (str "/ns/" (artifact 0) \/ (artifact 1) \/ (:name namespace) ".html")]
           (for [var (sorted-public-vars namespace)]
             (var-docs project namespace ns-href cljs? var)))
         [:div.endpage]]))

(defn- write-index [output-dir project artifact]
  (spit (io/file output-dir "index.html") (index-page project artifact)))

(defn- write-namespaces
  [output-dir project artifact]
  (doseq [namespace (:namespaces project)]
    (let [cljs? (:cljs namespace)]
      (spit (ns-filepath cljs? output-dir namespace)
            (namespace-page cljs? project namespace artifact)))))

(defn write-docs
  "Take raw documentation info and turn it into formatted HTML."
  [project artifact]
  (doto (:output-dir project)
    (write-index project artifact)
    (write-namespaces project artifact))
  nil)
