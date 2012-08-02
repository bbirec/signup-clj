(ns signup.core
  (:use [noir.core :only (defpage defpartial render)])
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


;; Model definition
(ds/defentity Sheet [title, desc, end-msg, data, slot, book, created-time, modified-time])

(defmacro defmodel [name properties]
  `(ds/defentity ~name [^:key
                        ~@(map (fn [p]
                                 (let [name (first p)] name))
                               properties)]))
                                   
(defmodel sheet
  [[title "Title" :string]
   [desc "Description" :text]
   [end-msg "Congraturation Message" :text]
   [data "Information" :json [:string]]
   [slot "Slot" :json [:string :integer]]
   [book "Book" :json [[:string]]]
   [created-time "Created Time" :created-time]
   [modified-time "Modified Time" :modified-time]])

(defn add-sheet []
  (ds/save! (Sheet. "title" "Desc" "Endmsg" nil nil nil 1 2)))

;; View definition

(defmacro defview [name models & body])



(defmacro render-view [view models]
  `"")


(defn show [model property &rest rest])

(defview add-sheet [sheet]
  [:h2 "Make Your SignUp Form"]
  (show sheet :title)
  (show sheet :desc)
  [:p "Information"]
  
  [:p "Slot"]
  
  (show sheet :end-msg)
  (save-button))


(defview list-sheet [sheet]
  [:tr
   [:td (show sheet :title)]
   [:td (show sheet :desc)]
   [:td (show sheet :created-time)]])


(defpartial list-sheet-table [cursor]
  ;; Getting the sheets from ds
  (if (= cursor nil) [:p "Empty cursor"] [:p cursor])
  
  [:table {:border "1"}
   [:tr
    [:td "Title"] [:td "Description"] [:td "Created Time"]]
   (render-view list-sheet model)])
  

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
    body (vali/on-error (keyword name) error-item)]])
  
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



(defpartial main-page [{:keys [title desc info slot final]}]
  (base
   [:h1 "Make your signup form"]
   [:div {:class "well"}
    (make-form [["Title" :string "title" title]
                ["Description" :text "desc" desc]
                ["Information" :json "info" info]
                ["Slot" :json "slot" slot]
                ["Final Message" :text "final" final]]
               "Create Signup Form")]))



(defpartial signup-view [{:keys [title desc info slot final book]}]
  (let [info (if (empty? info) [] (read-json info))
        slot (if (empty? slot) [] (read-json slot))]
  (base
   [:div {:class "well"}
    (with-form
      [:h1 title]
      [:p desc]
      [:h2 "Required Information"]
      (for [[title name]
            (map #(list %1 (str "info_" %2)) info (range (count info)))]
        (form-element title :string name ""))
      
      [:h2 "Slots"]
      (with-form-element "Available slots" :slot
        (for [[title limit value]
              (map #(conj %1 (str %2)) slot (range (count slot)))]
          [:div [:input {:type "radio" :name "slot" :value value} title]]))
      (form-buttons :submit-button "Sign Up"))])))


(defn valid? [{:keys [title desc info slot final]}]
  (vali/rule (vali/has-value? title)
             [:title "You must have title"])
  (vali/rule (vali/has-value? desc)
             [:desc "You must describe your signup form"])
  (vali/rule (vali/has-value? final)
             [:final "You must set your final message"])
  (not (vali/errors? :title :desc :final)))



;; Page Routing

(defpage "/" [:as param]
  (main-page param))

(defpage [:post "/"] {:as param}
  (if (valid? param)
    (do (add-sheet)
        (base [:div "Your signup form is created."]))
    (render "/" param)))

(defpage [:get ["/:sheet-key"]] {:keys [sheet-key]}
  (let [sheet (ds/retrieve Sheet sheet-key)]
    (if sheet
      (signup-view {:title "Title is here"
                    :desc "This is a test signup form for users"
                    :info "[\"Group name\", \"Phone number\"]"
                    :slot "[[\"5 PM ~ 6 PM\", 1], [\"6 PM ~ 7 PM\", 2]]"})
      (str "Invalid sheet key: " sheet-key))))

(defpage [:post "/:sheet-key"] {:as param}
  (base "Hooray!" (str param)))
  
;;;;;;;;;;;  
  
(defpage [:post "/login"] {:keys [username password]}
  (str "Logging in as " username " with the password " password))

(defpage [:get ["/login/:id" :id #"\d+"]] {:keys [id]}
  (str "You are " id "."))

(defpage "/error" []
  {:status 500
   :body "Oh no!"})

(defpage [:get ["/list"] ["/list:cursor"]] {:keys [cursor]}
  (base (list-sheet-table cursor)))



;; Registering the handler
(ae/def-appengine-app signup-app
  (noir-gae/gae-handler {}))
