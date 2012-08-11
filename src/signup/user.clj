(ns signup.user
  (:use signup.view)
  (:require [noir.session :as session])
  (:require [noir.validation :as vali])
  (:use [noir.response :only (redirect)])
  (:use [noir.core :only (defpage defpartial render)])
  (:require [appengine-magic.services.datastore :as ds]))


(ds/defentity User [^:key email, password])

(defn valid? [{:keys [email passwd1 passwd2]}]
  (vali/rule (vali/is-email? email)
             [:email "You must type your email address."])
  (vali/rule (vali/has-value? passwd1)
             [:passwd1 "Enter your password"])
  (vali/rule (vali/has-value? passwd2)
             [:passwd2 "Enter your password"])
  (vali/rule (= passwd1 passwd2)
             [:passwd2 "Enter the same password"])
  (not (vali/errors? :email :passwd1 :passwd2)))



(defpage "/register" {:keys [email passwd1 passwd2]}
  (base [:div {:class "well"}
         [:h2 "Register your account"]
         (with-form {}
           (with-form-element "Email"
             :email
             [:input {:type "text" :name "email" :value email}])
           (with-form-element "Password"
             :passwd1
             [:input {:type "password" :name "passwd1" :value passwd1}])
           (with-form-element "Password Confirm"
             :passwd2
             [:input {:type "password" :name "passwd2" :value passwd2}])
           (form-buttons :submit-button "Register"))]))


  
(defpage [:post "/register"] {:keys [email passwd1 passwd2] :as param}
  ;; Password check
  (if (valid? param)
    (ds/with-transaction
      (let [entity (ds/retrieve User email)]
        (if entity
          "You are already signed up."
          (do
            (ds/save! (User. email passwd1))
            "Thanks..."))))
    (render "/register" param)))

                    
(defpage "/login" {:keys [email passwd]}
  (base [:div {:class "well"}
         [:h2 "Login"]
         (with-form {}
           (with-form-element "Email"
             :email
             [:input {:type "text" :name "email" :value email}])
           (with-form-element "Password"
             :passwd
             [:input {:type "password" :name "passwd" :value passwd}])
           (form-buttons :submit-button "Login"))]))
                     
(defn valid-login? [{:keys [email passwd]}]
  (vali/rule (vali/is-email? email)
             [:email "Fill your email address"])
  (vali/rule (vali/has-value? passwd)
             [:passwd "Fill your password"])
  (not (vali/errors? :email :passwd)))

(defn valid-account? [entity {:keys [email passwd]}]

  (vali/rule (not (nil? entity))
             [:email "Invalid email address"])
  (if entity
    (vali/rule (= (get entity :password) passwd)
               [:passwd "Invalid password"]))
  (not (vali/errors? :email :passwd)))

(defn login [email]
  (session/put! :email email))

(defn logout []
  (session/clear!))

(defpage [:post "/login"] {:keys [email passwd] :as param}
  (if (valid-login? param)
    (let [entity (ds/retrieve User email)]
      (if (valid-account? entity param)
        (do (login email)
            (redirect "/"))
        (render "/login" param)))
    (render "/login" param)))
        
      
(defpage "/logout" []
  (logout)
  (redirect "/"))

