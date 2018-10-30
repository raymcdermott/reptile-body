(ns reptile.reload-server
  (:require [reptile.server.http :as http]
            [clojure-watch.core :refer [start-watch]]
            [clojure.java.io :as io]))

(defn start-server
  [port secret]
  (let [current-thread (Thread/currentThread)
        cl             (.getContextClassLoader current-thread)] ; DynamicClassLoader supports add-lib
    (.setContextClassLoader current-thread (clojure.lang.DynamicClassLoader. cl))
    (http/start-reptile-server port secret)))

(defn absolute-path
  [path]
  (.getCanonicalPath (io/file path)))

(defn boot-and-watch-fs!
  [path port secret]
  (letfn [(start-reptile [reptile-port shared-secret]
            (start-server reptile-port shared-secret))]
    (start-watch
      [{:path        (absolute-path path)
        :event-types [:create :modify :delete]
        :bootstrap   (fn [path]
                       (start-reptile port secret))
        :callback    (fn [event filename]
                       (when-not (.isDirectory (io/file filename))
                         ; We are cheating here . . .
                         (load-file (absolute-path (str path "/reptile/server/socket_repl.clj")))
                         (load-file (absolute-path (str path "/reptile/server/http.clj")))
                         (http/stop!)
                         (start-reptile port secret)))
        :options     {:recursive true}}])))
