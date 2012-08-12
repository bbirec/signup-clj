(ns signup.user
  (:use signup.view)
  (:require [noir.session :as session])
  (:require [noir.validation :as vali])
  (:use [noir.response :only (redirect)])
  (:use [noir.core :only (defpage defpartial render)])
  (:require [appengine-magic.services.datastore :as ds]))

(defn login [email]
  (session/put! :email email))

(defn logout []
  (session/clear!))

(defn get-email []
  (session/get :email))

(defn logged-in? []
  (not (nil? (get-email))))

(defmacro with-login-required [& body]
  `(if (logged-in?)
    (do ~@body)
    (redirect "/login")))

(defmacro with-login-not-required [& body]
  `(if (logged-in?)
     (redirect "/")
     (do ~@body)))

(defmacro with-permission-required [entity & body]
  `(if (= (get ~entity :email) (get-email))
     (do ~@body)
     (error-view "Access Denied." [:div "You don't have permission."])))


(defpartial base-user
  [title header & body]
  (layout title
           header
           (navbar "Signup Form"
                   (if (logged-in?)
                     [["Home" "/"]
                      ["Logout" "/logout"]]
                     [["Home" "/"]
                      ["Login" "/login"]
                      ["Register" "/register"]]))

           [:div {:class "container"} body
            [:hr]
            [:footer [:p "Company 2012"]]]))


(defpartial base-with-nav
  [& body]
  (base-user "Signup Web Site"
             [:script "var editable=true;"]
             body))

(defpartial base-with-nav-noneditable
  [& body]
  (base-user "Signup Web Site"
             [:script "var editable=false;"]
             body))

  

(ds/defentity User [^:key email, password])

(defn valid-register? [{:keys [email passwd1 passwd2]}]
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
  (with-login-not-required
    (base-with-nav
      [:div {:class "well"}
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
         (form-buttons :submit-button "Register"))])))


  
(defpage [:post "/register"] {:keys [email passwd1 passwd2] :as param}
  (with-login-not-required
    ;; Password check
    (if (valid-register? param)
      (ds/with-transaction
        (let [entity (ds/retrieve User email)]
          (if entity
            (error-view "You are already signed up."
                        "Please try to log-in")
            (do
              (ds/save! (User. email passwd1))
              (base-with-nav
                [:div {:class "well"}
                 [:h1 "Congraturation!"]
                 [:p "Registered successfully."]
                 [:p [:a {:class "btn btn-success"
                          :href "/login"} "Login"]]])))))
      (render "/register" param))))

                    
(defpage "/login" {:keys [email passwd]}
  (with-login-not-required
    (base-with-nav
      [:div {:class "well"}
       [:h2 "Login"]
       (with-form {}
         (with-form-element "Email"
           :email
           [:input {:type "text" :name "email" :value email}])
         (with-form-element "Password"
           :passwd
           [:input {:type "password" :name "passwd" :value passwd}])
         (form-buttons :submit-button "Login"))
       [:div "Your first visit?, " [:a {:href "/register"} "Register Now"]]])))
                     
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



(defpage [:post "/login"] {:keys [email passwd] :as param}
  (with-login-not-required
    (if (valid-login? param)
      (let [entity (ds/retrieve User email)]
        (if (valid-account? entity param)
          (do (login email)
              (redirect "/"))
          (render "/login" param)))
      (render "/login" param))))
        
      
(defpage "/logout" []
  (with-login-required
    (logout)
    (redirect "/")))

