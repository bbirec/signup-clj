(ns signup.httpsession
  (:import
    (java.io ByteArrayInputStream ByteArrayOutputStream)
    (java.io ObjectInputStream ObjectOutputStream))
  (:use ring.middleware.session.store))

(declare ^:dynamic *session*)

(defn serialize [obj]
  (let [bos (ByteArrayOutputStream.)]
    (with-open [oos (ObjectOutputStream. bos)]
      (.writeObject oos obj))
    (.toByteArray bos)))

(defn deserialize [bytes]
  (let [bis (ByteArrayInputStream. bytes)]
    (with-open [ois (ObjectInputStream. bis)]
      (.readObject ois))))

(deftype HttpSessionStore [session-key]
  SessionStore
  (read-session [_ _]
    (let [val (.getAttribute *session* session-key)]
      (if val (deserialize val) {})))
  (write-session [_ key data]
    (.setAttribute *session* session-key (serialize data))
    key)
  (delete-session [_ _]
    (.removeAttribute *session* session-key)
    nil))

(defn http-session-store [session-key]
  (HttpSessionStore. session-key))

(defn wrap-http-session-store
  [handler]
  (fn [request]
    (binding [*session* (.getSession (:request request))]
      (handler request))))