(ns signup.core
  (:use signup.view)
  (:use signup.user)
  (:require [signup.requtil :as requtil])
  (:require [signup.httpsession :as hs])
  (:use [noir.core :only (defpage defpartial render)])
  (:use [noir.response :only (redirect)])
  (:use [clojure.data.json :only (read-json json-str)])
  (:require [appengine-magic.core :as ae])
  (:require [appengine-magic.services.datastore :as ds])
  (:require [noir.util.gae :as noir-gae])
  (:require [noir.validation :as vali])
  (:require [noir.session :as session])
  (:use [hiccup.core])
  (:use [hiccup.page-helpers])
  (:import [com.google.appengine.api.datastore KeyFactory]))


;; Server logic

(defn gen-key [length]
  (apply str
         (take length
               (repeatedly #(rand-nth "1234567890")))))

(defn vec-nil [n]
  (for [_ (range n)] nil))


;; Model definition
(ds/defentity Sheet [^:key code, email, title, desc, final, info, slot, book, created-time])

(defn entity-to-map
  [e]
  (if e
    (zipmap (keys e) (vals e))
    nil))

(defn validate-book [book slot-size]
  (let [diff (- slot-size (count book))]
    (if (> diff 0)
      (vec (concat book (vec-nil diff)))
      book)))


(defn sheet-entity [e]
  "Transform the sheet entity to the map structure with parsed json"
  (let [em (entity-to-map e)]
    (let [sheet (reduce #(assoc %1 %2 (if (empty? (em %2))
                                        [] (read-json (em %2))))
                        em
                        [:info :slot :book])]
      (assoc sheet :book
             (validate-book (sheet :book) (count (sheet :slot)))))))

(defn get-sheet [sheet-key]
  (ds/retrieve Sheet sheet-key))

(defn get-sheet-by-key [k]
  (let [e (get-sheet k)]
    (if e
      (sheet-entity e)
      nil)))

(defmacro with-sheet
  ([key [entity map] then else]
     `(if-let [~entity (get-sheet ~key)]
        (if-let [~map (sheet-entity ~entity)]
          ~then
          ~else)
        ~else))
  ([key [entity map] then]
     `(with-sheet ~key [~entity ~map] ~then
        (error-view "Not found signup form" [:p "Please check again"]))))


    
;; Make signup form

(defn signup-form [button-text {:keys [title desc info slot final]}]
  [:div {:class "well"}
   (make-form
    {:id "signup-form"}
    [["Title" :string "title" title]
     ["Description" :text "desc" desc]
     ["Information" :json "info" info]
     ["Slot" :json "slot" slot]
     ["Final Message" :text "final" final]]
    button-text)])




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
  (let [key (gen-key 5)
        slot-count (count (read-json slot))
        empty-book (json-str (vec-nil slot-count))
        email (get-email)]
    (ds/save! (Sheet. key email title
                      desc final info slot
                      empty-book (java.util.Date.)))
    key))
  
(defn modify-sheet [entity new-values]
  (ds/save! (merge entity new-values)))

    
;; Booking


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

(defn except [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn delete-book [entity sheet slot-idx book-idx]
  (let [old-book (sheet :book)
        slot (except book-idx (get old-book slot-idx))
        new-book (assoc old-book slot-idx slot)]
    (save-book entity new-book)))


;; List of sheet

(defn get-sheets []
  (ds/query :kind Sheet
            :sort [[:created-time :dsc]]))


;; Patials



(defpartial signed-up-view [{:keys [slot final]} param]
  (base
   [:div {:class "well"}
    [:h1 "Congraturation!"]
    [:p "You signed up for "
     (let [slot-num (Integer/parseInt (param :slot))]
       (first (nth slot slot-num)))]
    [:p final]]))


(defpartial new-view [param]
  (base-with-nav
   [:h1 "Make your signup form"]
   (signup-form "Create Signup Form" param)))



(defpartial signup-view [{:keys [title desc final info slot book]}
                         param]
  (base
   [:div {:class "well"}
    (with-form {}
      [:h1 title]
      [:p desc]
      [:h2 "Required Information"]
      (for [[title name]
            (map #(list %1 (str "info_" %2))
                 info (range (count info)))]
        (form-element title :string name (get param (keyword name))))
      
      [:h2 "Slots"]
      (with-form-element "Available slots" :slot
        (for [[title limit value checked disabled available]
              (map #(conj %1
                          (str %2)
                          (if (empty? (param :slot)) false
                              (= %2 (Integer/parseInt (param :slot))))
                          (<= (second %1) (count %3))
                          (- (second %1) (count %3)))
                   slot
                   (range (count slot))
                   book)]
          [:div
           [:input
            (let [prop {:type "radio" :name "slot" :value value}
                  prop-check (if checked
                               (assoc prop :checked "checked")
                               prop)
                  prop-disable (if disabled
                                 (assoc prop-check :disabled "disabled")
                                 prop-check)]
              prop-disable)
            title]
           (if disabled
             [:span " - Not available"]
             [:span (str " - " available " slot(s) available.")])]))
      (form-buttons :submit-button "Sign Up"))]))


(defpartial sheet-list-view []
  [:div
   [:table {:class "table table-striped"}
    [:thead
     [:tr
      [:th "URL"]
      [:th "Title"]
      [:th "Created time"]
      [:th "Status"]
      [:th "Edit"]
      [:th "Delete"]]]
    [:tbody
     (for [sheet (get-sheets)]
       (let [entity (sheet-entity sheet)]
         [:tr
          [:td [:a {:href (str "/" (entity :code)) :target "_blank"}
                (requtil/absolute-url (str "/" (entity :code)))]]
          [:td (entity :title)]
          [:td (str (entity :created-time))]
          [:td [:a {:href (str "/" (entity :code) "/status")
                    :class "btn btn-primary"} "View Status"]]
          [:td [:a {:href (str "/" (entity :code) "/edit")
                    :class "btn btn-info"} "Edit"]]
          [:td [:a {:href (str "/" (entity :code) "/delete")
                    :class "btn btn-danger"} "Delete"]]]))]]])




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
                     (str "/"
                          code
                          "/delete-book?slot-idx="
                          slot-idx
                          "&book-idx="
                          book-idx)} "Delete"]]])))]])


(defpartial homepage []
  (base-with-nav
    (let [email (get-email)]
      (if email
        [:div 
         [:p (str "Hi, " email)]
         (sheet-list-view)
         [:a {:class "btn btn-large btn-primary"
              :href "/new"}
          "Create New Signup Form"]]
        [:div {:class "hero-unit"}
         [:h1 "Signup form"]
         [:p "This is a template for a simple marketing or ....."]]))))

(defpartial signup-modified-view [code]
  (base [:div {:class "well"}
         [:h1 "Signup form is modified"]
         [:p "Signup form : "
          [:a {:href (str "/" code)} "here"]]]))



;; Pages

(defpage "/" [:as param]
  (homepage))

(defpage "/new" [:as param]
  (with-login-required
    (new-view param)))


(defpage [:post "/new"] {:as param}
  (with-login-required
    (if (valid? param)
      (do (let [key (add-sheet param)]
            (base-with-nav
              [:div {:class "well"}
               [:h1 "Your signup form is created."]
               [:p "Signup form : " [:a {:href (str "/" key)} "here"]]])))
      (render "/new" param))))


(defpage "/:sheet-key" {:keys [sheet-key] :as param}
  (with-sheet sheet-key [entity sheet]
    (signup-view sheet param)))


(defpage [:post "/:sheet-key"] {:keys [sheet-key] :as param}
  (ds/with-transaction 
    (with-sheet sheet-key [entity sheet]
      (if (valid-signup? sheet param)
        (let [slot-idx (Integer/parseInt (param :slot))]
          (if (has-room? sheet slot-idx)
              (do (add-book entity sheet param slot-idx)
                  (signed-up-view sheet param))
              (error-view "The slot is full"
                          "Please select another slot.")))
        (render "/:sheet-key" param)))))


(defpage "/:sheet-key/edit" {:keys [sheet-key]}
  (with-sheet sheet-key [entity sheet]
    (with-permission-required entity
      (base-with-nav-noneditable
        [:h1 "Edit Form"]
        (signup-form "Edit" entity)))))


(defpage [:post ["/:sheet-key/edit"]] {:keys [sheet-key] :as param}
  (if (valid? param)
    (ds/with-transaction
      (with-sheet sheet-key [entity sheet]
        (with-permission-required entity
          (do (modify-sheet entity param)
              (signup-modified-view (sheet :code))))))
    (render "/:sheet-key/edit" param)))


(defpage "/:sheet-key/status" {:keys [sheet-key]}
  (with-sheet sheet-key [entity sheet]
    (with-permission-required entity
      (base-with-nav
        [:h1 "Status"]
        [:div (view-book-table sheet)]))))


(defpage "/:sheet-key/delete-book" {:keys [sheet-key slot-idx book-idx]}
  (ds/with-transaction
    (with-sheet sheet-key [entity sheet]
      (with-permission-required entity
        (do (delete-book entity
                         sheet
                         (Integer/parseInt slot-idx)
                         (Integer/parseInt book-idx))
            (redirect (str "/" sheet-key "/status")))))))

(defpage "/:sheet-key/delete" {:keys [sheet-key]}
  (ds/with-transaction
    (with-sheet sheet-key [entity sheet]
      (with-permission-required entity
        (ds/delete! entity)
        (redirect "/")))))





;; Registering the handler
(ae/def-appengine-app signup-app
  (hs/wrap-http-session-store
   (requtil/wrap-requtil
    (noir-gae/gae-handler
     {:session-store (hs/http-session-store "signup-session")}))))
