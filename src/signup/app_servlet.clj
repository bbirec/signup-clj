(ns signup.app_servlet
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use signup.core)
  (:use [appengine-magic.servlet :only [make-servlet-service-method]]))


(defn -service [this request response]
  ((make-servlet-service-method signup-app) this request response))
