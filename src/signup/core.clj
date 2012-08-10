(ns signup.core
  (:use [noir.core :only (defpage defpartial render)])
  (:use [noir.response :only (redirect)])
  (:use [clojure.data.json :only (read-json json-str)])
  (:require [appengine-magic.core :as ae])
  (:require [appengine-magic.services.datastore :as ds])
  (:require [noir.util.gae :as noir-gae])
  (:require [noir.validation :as vali])
  (:use [hiccup.core])
  (:use [hiccup.page-helpers]))


;; Client HTML
(defpartial layout [title head body]
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    [:title title]
    ;; JQuery
    (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")
    ;; Bootstrap
    (include-css "/assets/css/bootstrap.css")
    (include-css "/assets/css/bootstrap-responsive.css")
    head]
   [:body
    body]))

(defpartial container [& body]
  [:div {:class "container"} body])


;; Server logic

(defn gen-key [length]
  (apply str
         (take length
               (repeatedly #(rand-nth "1234567890")))))

;; Model definition
(ds/defentity Sheet [^:key code, title, desc, final, info, slot, book, created-time])

(defn entity-to-map
  [e]
  (if e
    (zipmap (keys e) (vals e))
    nil))

(defn sheet-entity [e]
  "Transform the sheet entity to the map structure with parsed json"
  (let [em (entity-to-map e)]
    (reduce #(assoc %1 %2 (if (empty? (em %2))
                            [] (read-json (em %2))))
            em
            [:info :slot :book])))

(defn get-sheet [sheet-key]
  (ds/retrieve Sheet sheet-key))

(defn get-sheet-by-key [k]
  (let [e (get-sheet k)]
    (if e
      (sheet-entity e)
      nil)))
    
(defmacro with-sheet [key [entity map] & body]
  `(let [~entity (get-sheet ~key)
         ~map (sheet-entity ~entity)]
     ~@body))


(defmacro defmodel [name properties]
  `(ds/defentity ~name [~@(map (fn [p]
                                 (let [name (first p)] name))
                               properties)]))
                                   
(defmodel Shoot
  [[^:key title "Title" :string]
   [desc "Description" :text]
   [end-msg "Congraturation Message" :text]
   [data "Information" :json [:string]]
   [slot "Slot" :json [:string :integer]]
   [book "Book" :json [[:string]]]
   [created-time "Created Time" :created-time]
   [modified-time "Modified Time" :modified-time]])





(defpartial base [& body]
  (layout "SignUp Website"
          ""
          (container body)))

(defpartial error-item [[first-error]]
  [:span {:class "help-inline"} first-error])


(defpartial with-form [& body]
  [:form {:method "post" :class "form-horizontal"}
   [:fieldset
    body]])


(defpartial form-buttons [& {:keys [submit-button reset-button]
                             :or {submit-button "Submit" reset-button "Reset"}}]
  [:div {:class "form-actions"}
   [:button {:type "submit" :class "btn btn-primary"} submit-button]
   [:button {:class "btn" :type "reset"} reset-button]])

(defpartial with-form-element [title error-keyword & body]
  [:div {:class (if (vali/get-errors error-keyword)
                  "control-group error"
                  "control-group")}
   [:label {:class "control-label"} title]
   [:div {:class "controls"}
    body (vali/on-error error-keyword error-item)]])
  
(defpartial form-element [title type name value]
  (with-form-element title (keyword name)
    (cond (= type :string) [:input {:type "text" :name name :value value}]
          (= type :password) [:input {:type "password" :name name :value value}]
          (= type :text) [:textarea {:name name} value]
          (= type :json) [:textarea {:name name} value])))


(defn make-form [elements button-text]
  (with-form
    (for [[title type name value] elements] (form-element title type name value))
    (if button-text (form-buttons :submit-button button-text))))

(defn signup-form [button-text {:keys [title desc info slot final]}]
  [:div {:class "well"}
    (make-form [["Title" :string "title" title]
                ["Description" :text "desc" desc]
                ["Information" :json "info" info]
                ["Slot" :json "slot" slot]
                ["Final Message" :text "final" final]]
               button-text)])

(defpartial main-page [param]
  (base
   [:h1 "Make your signup form"]
   (signup-form "Create Signup Form" param)))



(defpartial signup-view [{:keys [title desc final info slot book]}
                         param]
  (base
   [:div {:class "well"}
    (with-form
      [:h1 title]
      [:p desc]
      [:h2 "Required Information"]
      (for [[title name]
            (map #(list %1 (str "info_" %2))
                 info (range (count info)))]
        (form-element title :string name (get param (keyword name))))
      
      [:h2 "Slots"]
      (with-form-element "Available slots" :slot
        (for [[title limit value checked]
              (map #(conj (conj %1 (str %2))
                          (if (empty? (param :slot)) false
                              (= %2 (Integer/parseInt (param :slot)))))
                   slot (range (count slot)))]
          [:div
           [:input
            (if checked
              {:type "radio" :name "slot" :value value :checked "checked"}
              {:type "radio" :name "slot" :value value})
            title]]))
      (form-buttons :submit-button "Sign Up"))]))

(defn valid-json [json-str]
  (let [json (try (read-json json-str) (catch Exception e nil))]
    json))

    
(defn valid? [{:keys [title desc final info slot]}]
  (vali/rule (vali/has-value? title)
             [:title "You must have title"])
  (vali/rule (vali/has-value? desc)
             [:desc "You must describe your signup form"])
  (vali/rule (vali/has-value? final)
             [:final "You must set your final message"])
  (vali/rule (valid-json info)
             [:info "Invalid information definition"])
  (vali/rule (valid-json slot)
             [:slot "Invalid slot definition"])
  (not (vali/errors? :title :desc :final :info :slot)))


(defn add-sheet [{:keys [title desc final info slot]}]
  (let [key (gen-key 5)]
    (ds/save! (Sheet. key title desc final info slot nil (java.util.Date.)))
    key))
  
(defn modify-sheet [sheet-key new-values]
  (ds/with-transaction
    (let [e (get-sheet sheet-key)]
      (if e
        (do
          (ds/save! (merge e new-values))
          (get e :code))
        nil))))
    


;; Page Routing

(defpage "/" [:as param]
  (main-page param))

(defpage [:post "/"] {:as param}
  (if (valid? param)
    (do (let [key (add-sheet param)]
          (base [:div
                 [:h1 "Your signup form is created."]
                 [:p "Signup form : " [:a {:href (str "/" key)} "here"]]])))
    (render "/" param)))


(defpage "/:sheet-key" {:keys [sheet-key] :as param}
  (with-sheet sheet-key [entity sheet]
    (if sheet
      (signup-view sheet param)
      (str "Invalid sheet key: " sheet-key))))

(defpartial signed-up-view [{:keys [slot final]} param]
  (base
   [:div {:class "well"}
    [:h1 "Congraturation!"]
    [:p "You signed up for "
     (let [slot-num (Integer/parseInt (param :slot))]
       (first (nth slot slot-num)))]
    [:p final]]))

(defn is-int-str? [s]
  (if (empty? s) nil
      (re-matches #"[0-9]+" s)))

(defn get-info-keys [sheet]
  (map #(keyword (str "info_" %1)) (range (count (sheet :info)))))

(defn valid-signup? [sheet {:keys [slot] :as param}]
  (let [keys (get-info-keys sheet)]
    (vali/rule (is-int-str? slot)
               [:slot "You should check one of available slots."])

    (doseq [key keys]
      (vali/rule (vali/has-value? (get param key))
                 [key "Fill this blank"]))
    
    (not (apply vali/errors? :slot keys))))

(defn save-book [entity book]
  (let [book-string (json-str book)]
    (ds/save! (assoc entity :book book-string))))


(defn add-book [entity sheet param slot-idx]
  (let [keys (get-info-keys sheet)
        values (for [k keys] (param k))
        old-book (sheet :book)
        slot (conj (get old-book slot-idx) values)
        new-book (assoc old-book slot-idx slot)]
    (save-book entity new-book)))
        


(defn has-room? [sheet slot-idx]
  (let [limit (second (get (sheet :slot) slot-idx))
        current (count (get (sheet :book) slot-idx))]
    (< current limit)))
    

(defpage [:post "/:sheet-key"] {:keys [sheet-key] :as param}
  (ds/with-transaction 
    (with-sheet sheet-key [entity sheet]
      (if entity
        (if (valid-signup? sheet param)
          (let [slot-idx (Integer/parseInt (param :slot))]
            (if (has-room? sheet slot-idx)
              (do (add-book entity sheet param slot-idx)
                (signed-up-view sheet param))
              (str "There is no room.")))
          (render "/:sheet-key" param))
        (str "Invalid sheet key: " sheet-key)))))

(defn get-sheets []
  (ds/query :kind Sheet
            :sort [[:created-time :dsc]]))
  

(defpage [:get ["/user/:user-id"]] {:keys [user-id]}
  (base
   [:div "Hi, " user-id]
   [:div
    [:table {:class "table table-striped"}
     [:thead
      [:tr
       [:th "URL"]
       [:th "Title"]
       [:th "# of Slots"]
       [:th "Progress"]
       [:th "Created time"]
       [:th "Manage"]]]
     [:tbody
      (for [sheet (get-sheets)]
        (let [entity (sheet-entity sheet)]
          [:tr
           [:td [:a {:href (str "/" (entity :code))} (entity :code)]]
           [:td (entity :title)]
           [:td (str (count (entity :slot)))]
           [:td (str 0)]
           [:td "Now"]
           [:td [:a {:href (str "/manage/" (entity :code))} "Manage"]]]))]]]))


(defpartial view-book-table [{:keys [info slot book code]}]
  [:table {:class "table table-striped"}
   [:thead
    [:tr [:th "Slot"] (for [i info] [:th i]) [:th "Delete"]]]
   [:tbody
    (for [[title limit books slot-idx]
          (map #(conj %1 %2 %3)
               slot
               book
               (range (count slot)))]
      (if (empty? books)
        [:tr [:td title]] ;; Empty booking
        (for [[book book-idx] (map #(vector %1 %2) books (range (count books)))]
          [:tr
           ;; Slot
           (if (identical? (first books) book)
             [:td {:rowspan (str (count books))} title])
            
           (for [b book] [:td b])

           [:td [:a {:class "btn btn-danger"
                     :href
                     (str "/manage/"
                          code
                          "/delete?slot-idx="
                          slot-idx
                          "&book-idx="
                          book-idx)} "Delete"]]])))]])



(defpage "/manage/:sheet-key" {:keys [sheet-key]}
  (with-sheet sheet-key [entity sheet]
    (if sheet
      (base
       [:h1 "Status"]
       [:div (view-book-table sheet)]
       [:h1 "Edit Form"]
       (signup-form "Edit" entity)
      )

      (str "Sorry"))))

(defpage [:post ["/manage/:sheet-key"]] {:keys [sheet-key] :as param}
  (if (valid? param)
    (let [key (modify-sheet sheet-key param)]
      (if key
        (base [:div
               [:h1 "Signup form is modified"]
               [:p "Signup form : " [:a {:href (str "/" key)} "here"]]])
        (str "Sorry")))
    (render "/manage/:sheet-key" param)))

(defn except [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn delete-book [entity sheet slot-idx book-idx]
  (let [old-book (sheet :book)
        slot (except book-idx (get old-book slot-idx))
        new-book (assoc old-book slot-idx slot)]
    (save-book entity new-book)))

        
 

(defpage "/manage/:sheet-key/delete" {:keys [sheet-key slot-idx book-idx]}
  (ds/with-transaction
    (with-sheet sheet-key [entity sheet]
      (if entity
        (do (delete-book entity sheet (Integer/parseInt slot-idx) (Integer/parseInt book-idx))
            (redirect (str "/manage/" sheet-key)))
        (str "Not found")))))

  

;;;;;;;;;;;  
  
(defpage "/login" {:keys [username password]}
  (str "Logging in as " username " with the password " password))

(defpage [:get ["/login/:id" :id #"\d+"]] {:keys [id]}
  (str "You are " id "."))

(defpage "/error" []
  {:status 500
   :body "Oh no!"})


;; Registering the handler
(ae/def-appengine-app signup-app
  (noir-gae/gae-handler {}))
