(ns signup.view
  (:require [noir.validation :as vali])
  (:use [hiccup.page-helpers])
  (:use [noir.core :only (defpage defpartial render)]))


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

(defpartial base [& body]
  (layout "SignUp Website"
          ""
          (container body)))

(defpartial error-view [title & body]
  (base [:div {:class "well"}
         [:h1 title]
         [:div body]]))

;; Form


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
