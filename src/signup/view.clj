(ns signup.view
  (:require [noir.validation :as vali])
  (:use [hiccup.page-helpers])
  (:use [noir.core :only (defpage defpartial render)]))


(defpartial layout [title head & body]
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1.0"}]
    [:title title]
    ;; JQuery
    (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")
    ;; Bootstrap
    (include-css "/assets/css/bootstrap.css")
    [:style {:type "text/css"} "body {padding-top: 60px; padding-bottom: 40px;}"]    
    (include-css "/assets/css/bootstrap-responsive.css")
    (include-js "/assets/js/bootstrap-collapse.js")
    (include-js "/assets/js/json2.js")
    (include-js "/assets/js/signup.js")

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

(defpartial navbar [brand links]
  [:div {:class "navbar navbar-fixed-top"}
   [:div {:class "navbar-inner"}
    [:div {:class "container"}
     [:a {:class "btn btn-navbar"
          :data-toggle "collapse"
          :data-target ".nav-collapse"}
      (for [_ (range (count links))]
        [:span {:class "icon-bar"}])]
     
     [:a {:class "brand"} brand]
     
     [:div {:class "nav-collapse"}
      [:ul {:class "nav"}
       (for [[title url] links]
         [:li [:a {:href url} title]])]]]]])



;; Form

(defpartial error-item [[first-error]]
  [:span {:class "help-inline"} first-error])


(defpartial with-form [opts & body]
  [:form (merge opts {:method "post" :class "form-horizontal"})
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
          (= type :json) [:div {:name name :id name} value])))


(defn make-form [form-opts elements button-text]
  (with-form form-opts
    (for [[title type name value] elements] (form-element title type name value))
    (if button-text (form-buttons :submit-button button-text))))
