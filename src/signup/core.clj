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
  (:import com.google.appengine.api.datastore.Text)
  (:require [noir.util.gae :as noir-gae])
  (:require [noir.validation :as vali])
  (:require [noir.session :as session])
  (:use [hiccup.core])
  (:use [hiccup.page-helpers]))



;; Server logic

(defn gen-key [length]
  (apply str
         (take length
               (repeatedly #(rand-nth "1234567890")))))

(defn vec-nil [n]
  (for [_ (range n)] nil))

(defn replace-map [m keys func]
  (let [valid-keys (filter #(not (nil? (get m %))) keys)
        values (map #(func (get m %)) valid-keys)
        new-m (zipmap valid-keys values)]
    (merge m new-m)))

(def text-properties [:info :slot :desc :final :book])


(defn wrap-text
  ([param keys]
     (replace-map param keys #(Text. %)))
  ([param]
     (wrap-text param text-properties)))

(defn unwrap-text
  ([param keys]
     (replace-map param keys #(.getValue %)))
  ([param]
     (unwrap-text param text-properties)))
     


   
;; Model definition
(ds/defentity Sheet [^:key code, email, title, desc, final, info, slot, book, exit, created-time])

;; Test
(defn migration-string-to-text []
  (let [entities (ds/query :kind Sheet)
        new-entities (map #(wrap-text %) entities)]
    (ds/save! new-entities)))
        
(defpage "/mig1" {}
  (migration-string-to-text)
  "OK")


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
    (let [em (unwrap-text em)
          sheet (reduce #(assoc %1 %2 (if (empty? (em %2))
                                        [] (read-json (em %2))))
                        em
                        [:info :slot :book])]
      (assoc sheet :book
             (validate-book (sheet :book) (count (sheet :slot)))))))

(defn get-sheet [sheet-key]
  (ds/retrieve Sheet sheet-key))

(defn get-sheets [email]
  (ds/query :kind Sheet
            :filter (= :email email)
            :sort [[:created-time :dsc]]))


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
        (error-view "Not found sign-up form" [:p "Please check again"]))))


    
;; Make signup form





(defn parse-json [json-str]
  (let [json (try (read-json json-str) (catch Exception e nil))]
    json))


(defn valid-info? [json]
  (and
   (not (nil? json))
   (coll? json)))

(defn valid-info-string? [json]
  (every? #(and (string? %)
                (not= "" %))
          json))


(defn valid-slot? [json]
  (and
   (not (nil? json))
   (coll? json)
   (every? #(and (coll? %)
                 (= (count %) 2))
           json)))

(defn valid-slot-name? [json]
  (every? #(and (string? (first %))
                (not= "" (first %)))
          json))

(defn valid-slot-limit? [json]
  (every? #(and (integer? (second %))
                (pos? (second %)))
          json))


    
(defn valid? [{:keys [title desc final info slot exit]}]
  (vali/rule (vali/has-value? title)
             [:title "You must have title"])
  (vali/rule (vali/has-value? desc)
             [:desc "You must describe your sign-up form"])
  (vali/rule (vali/has-value? final)
             [:final "You must set your final message"])

  (let [info-json (parse-json info)
        slot-json (parse-json slot)]
    
    (and (vali/rule (valid-info? info-json)
                    [:info "Invalid information definition"])
         (vali/rule (valid-info-string? info-json)
                    [:info "Fill in all blanks."]))
    (and (vali/rule (valid-slot? slot-json)
                    [:slot "Invalid slot definition"])
         (vali/rule (valid-slot-name? slot-json)
                    [:slot "Fill in all blanks."])
         (vali/rule (valid-slot-limit? slot-json)
                    [:slot "Please write the positive number."])))

  (vali/rule (vali/has-value? exit)
             [:exit "You must set your exit url."])
  (not (vali/errors? :title :desc :final :info :slot :exit)))




(defn add-sheet [{:keys [title desc final info slot exit]}]
  (let [key (gen-key 5)
        slot-count (count (read-json slot))
        empty-book (json-str (vec-nil slot-count))
        email (get-email)]
    (ds/save! (Sheet. key email title
                      (Text. desc) (Text. final) (Text. info) (Text. slot)
                      (Text. empty-book) exit (java.util.Date.)))
    key))
  
(defn modify-sheet [entity new-values]
  (let [new-values (wrap-text new-values)]
    (ds/save! (merge entity new-values))))

    
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
                 [key "Fill in this blank"]))
    
    (not (apply vali/errors? :slot keys))))


(defn save-book [entity book]
  (let [book-string (json-str book)]
    (ds/save! (merge entity
                     (wrap-text {:book book-string})))))


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


;; Patials

(defn signup-form [button-text {:keys [title desc info slot final exit]}]
  [:div {:class "well"}
   (make-form
    {:id "signup-form"}
    [["Title" :string "title" title]
     ["Description" :text "desc" (h desc)]
     ["Information" :json "info" (h info)]
     ["Slot" :json "slot" (h slot)]
     ["Final Message" :text "final" (h final)]
     ["Exit Url" :url "exit" exit]]
    button-text)])


(defpartial signed-up-view [{:keys [code slot final exit]} param]
  (base
   [:div {:class "well"}
    [:h1 "Congratulations!"]
    [:h4 "You have successfully signed up for "
     (let [slot-num (Integer/parseInt (param :slot))]
       (h (first (nth slot slot-num))))
     "."]
    [:h4 (h final)]
    [:p [:a {:class "btn btn-success"
           :href (str "http://" exit)} "Click Here to Exit"]]]))


(defpartial new-view [param]
  (base-with-nav
   [:h1 "Create a New Sign-Up Form"]
   (signup-form "Create" param)))



(defpartial signup-view [{:keys [title desc final info slot book]}
                         param]
  (base
   [:div {:class "well"}
    (with-form {}
      [:h1 (h title)]
      [:h4 (h desc)]
      [:h2 "Required Information"]
      (for [[title name]
            (map #(list %1 (str "info_" %2))
                 info (range (count info)))]
        (form-element (h title) :string name
                      (h (get param (keyword name)))))
      
      [:h2 "Sign-up for"]
      (with-form-element "" :slot
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
            (let [prop {:type "radio" :name "slot" :value (h value)}
                  prop-check (if checked
                               (assoc prop :checked "checked")
                               prop)
                  prop-disable (if disabled
                                 (assoc prop-check :disabled "disabled")
                                 prop-check)]
              prop-disable)
            (h title)]
           (if disabled
             [:span " - Not available"]
             [:span (str " - " available " slot(s) available.")])]))
      (form-buttons :submit-button "Sign-Up"))]))


(defpartial sheet-list-view [email]
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
     (for [sheet (get-sheets email)]
       (let [entity (sheet-entity sheet)]
         [:tr
          [:td [:a {:href (str "/" (entity :code)) :target "_blank"}
                (requtil/absolute-url (str "/" (entity :code)))]]
          [:td (h (entity :title))]
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
    [:tr [:th "Slot"] (for [i info] [:th (h i)]) [:th "Delete"]]]
   [:tbody
    (for [[title limit books slot-idx]
          (map #(conj %1 %2 %3)
               slot
               book
               (range (count slot)))]
      (if (empty? books)
        [:tr [:td (h title)]] ;; Empty booking
        (for [[book book-idx] (map #(vector %1 %2) books (range (count books)))]
          [:tr
           ;; Slot
           (if (identical? (first books) book)
             [:td {:rowspan (str (count books))} (h title)])
            
           (for [b book] [:td (h b)])

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
         (sheet-list-view email)
         [:a {:class "btn btn-large btn-primary"
              :href "/new"}
          "Create a New Sign-Up Form"]]
        [:div {:class "hero-unit"}
         [:h1 "Sign-Up Form"]
         [:p "Sign-Up form is ......"]
         [:p [:a {:class "btn btn-primary btn-large"
                  :href "/new"}
              "Learn More"]]]))))

(defpartial signup-modified-view [code]
  (base-with-nav
    [:div {:class "well"}
     [:h1 "Sign-Up form is modified"]
     [:p "Check your sign-up form : "
      [:a {:href (str "/" code)
           :target "_blank"}
       (requtil/absolute-url (str "/" code))]]
     [:p [:a {:class "btn btn-success"
              :href "/"}
          "Done"]]]))



;; Pages

(defpage "/" [:as param]
  (with-login-required
    (homepage)))

(defpage "/new" [:as param]
  (with-login-required
    (new-view param)))


(defpage [:post "/new"] {:as param}
  (with-login-required
    (if (valid? param)
      (do (let [key (add-sheet param)]
            (base-with-nav
              [:div {:class "well"}
               [:h1 "Your sign-up form is created."]
               [:p "Sign-Up form : "
                [:a {:href (str "/" key)
                     :target "_blank"}
                 (requtil/absolute-url (str "/" key))]]
               [:p [:a {:href "/"
                        :class "btn btn-success"} "Done"]]])))
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
        (signup-form "Edit" (unwrap-text entity))))))


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
