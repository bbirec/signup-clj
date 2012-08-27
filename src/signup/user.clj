(ns signup.user
  (:use signup.view)
  (:require [signup.requtil :as requtil])
  (:require [noir.session :as session])
  (:require [noir.validation :as vali])
  (:use [noir.response :only (redirect)])
  (:use [noir.core :only (defpage defpartial render)])
  (:require [noir.util.crypt :as crypt])
  (:require [appengine-magic.services.datastore :as ds])
  (:require [appengine-magic.services.mail :as mail]))

(defn login [email]
  (session/put! :email email))

(defn logout []
  (session/clear!))

(defn get-email []
  (session/get :email))

(defn logged-in? []
  (not (nil? (get-email))))

(defmacro if-logged-in [then else]
  `(if (logged-in?)
     ~then ~else))

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
           (navbar "Sign-Up Form"
                   (if (logged-in?)
                     [["My Sign-Up Forms" "/"]
                      ["Reset Password" "/reset"]
                      ["Logout" "/logout"]]
                     [["Home" "/"]
                      ["Login" "/login"]
                      ["Register" "/register"]]))

           [:div {:class "container"} body
            [:hr]
            [:footer [:p "Powered by Google AppEngine with Clojure"]]]))


(defpartial base-with-nav
  [& body]
  (base-user "Sign-Up Web Site"
             [:script "var editable=true;"]
             body))

(defpartial base-with-nav-noneditable
  [& body]
  (base-user "Sign-Up Web Site"
             [:script "var editable=false;"]
             body))

(defpartial info-view [title msg button-text button-link]
  (base-with-nav
    [:div {:class "well"}
     [:h1 title]
     [:h4 msg]
     [:a {:class "btn btn-info" :href button-link} button-text]]))

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
              (ds/save! (User. email (crypt/encrypt passwd1)))
              (base-with-nav
                [:div {:class "well"}
                 [:h1 "Congratulations!"]
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
       [:div "Your first visit?, " [:a {:href "/register"} "Register Now"]]
       [:div "Forgot your password?, " [:a {:href "/forgot"} "Reset password"]]])))
                     
(defn valid-login? [{:keys [email passwd]}]
  (vali/rule (vali/is-email? email)
             [:email "Fill your email address"])
  (vali/rule (vali/has-value? passwd)
             [:passwd "Fill your password"])
  (not (vali/errors? :email :passwd)))

(defn valid-account? [entity {:keys [email passwd]}]

  (vali/rule (not (nil? entity))
             [:email "Wrong email address"])
  (if entity
    (vali/rule (crypt/compare passwd (get entity :password))
               [:passwd "Wrong password"]))
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


(defpage "/reset" {:keys [current new1 new2]}
  (with-login-required
    (base-with-nav
      [:div {:class "well"}
       [:h2 "Reset Password"]
       (with-form {}
         (with-form-element "Current password"
           :current
           [:input {:type "password" :name "current" :value current}])
         (with-form-element "New password"
           :new1
           [:input {:type "password" :name "new1" :value new1}])
         (with-form-element "New password confirm"
           :new2
           [:input {:type "password" :name "new2" :value new2}])
         (form-buttons :submit-button "Change"))])))


(defn valid-reset? [{:keys [current new1 new2]}]
  (vali/rule (vali/has-value? current)
             [:current "Enter your current password."])
  (vali/rule (vali/has-value? new1)
             [:new1 "Enter new password."])
  (vali/rule (vali/has-value? new2)
             [:new2 "Enter the new password."])
  (vali/rule (= new1 new2)
             [:new2 "Enter the same password."])
  (not (vali/errors? :current :new1 :new2)))


(defn change-password [entity passwd]
  (ds/save! (assoc entity :password (crypt/encrypt passwd))))
             
(defpage [:post "/reset"] {:keys [current new1 new2] :as param}
  (with-login-required
    (ds/with-transaction
      (let [entity (ds/retrieve User (get-email))]
        (if (crypt/compare current (get entity :password))
          (if (valid-reset? param)
            (do (change-password entity new1)
                (redirect "/"))
            (render "/reset" param))
          (do (vali/set-error :current "Wrong password")
              (render "/reset" param)))))))

  

(defn random-password []
  (apply str
         (take 10
               (repeatedly #(rand-nth "1234567890")))))

(defn send-reset-mail [email new-passwd]
  (let [link (requtil/absolute-url "/login")
        msg (mail/make-message :from "bbirec@gmail.com"
                               :to email
                               :subject "Forgot password request for SignUp Form"
                               :html-body
                               (str "Your new password is : " new-passwd "<br/>"
                                    "Please reset your password after logging-in<br/>"
                                    "<a href=\"" link "\">Login Here</a>"))]
    (mail/send msg)))



(defpage "/forgot" {:keys [email]}
  (with-login-not-required
    (base-with-nav
      [:div {:class "well"}
       [:h2 "Reset Password"]
       (with-form {}
         (with-form-element "Your email address"
           :email
           [:input {:type "text" :name "email" :value email}])
         (form-buttons :submit-button "Reset Password"))])))

(defpage [:post "/forgot"] {:keys [email] :as param}
  (with-login-not-required
    (ds/with-transaction
      (if (vali/rule (vali/has-value? email)
                     [:email "Enter your email address."])
        (let [entity (ds/retrieve User email)]
          (if (nil? entity)
            (do (vali/set-error :email "Wrong email address.")
                (render "/forgot" param))
            (let [passwd (random-password)]
              (change-password entity passwd)
              (send-reset-mail email passwd)
              (info-view
               "New password has been sent!"
               "New password has been sent to your email. Please check your email inbox."
               "Okay"
               "/"))))
        (render "/forgot" param)))))
