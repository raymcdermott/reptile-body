(ns reptile.reload-server
  (:require [reptile.server.http :as http]
            [clojure-watch.core :refer [start-watch]]
            [clojure.java.io :as io]))

(defn start-server
  [port secret socket-host socket-port]
  (let [current-thread (Thread/currentThread)
        cl             (.getContextClassLoader current-thread)] ; DynamicClassLoader supports add-lib
    (.setContextClassLoader current-thread (clojure.lang.DynamicClassLoader. cl))
    (http/start-reptile-server port secret socket-host socket-port)))

(defn absolute-path
  [path]
  (.getCanonicalPath (io/file path)))

(defn boot-and-watch-fs!
  ([path port secret]
   (boot-and-watch-fs! path port secret :self 0))
  ([path port secret socket-host socket-port]
   (start-watch
     [{:path        (absolute-path path)
       :event-types [:create :modify :delete]
       :bootstrap   (fn [_]
                      (start-server port secret socket-host socket-port))
       :callback    (fn [_ filename]
                      (when-not (.isDirectory (io/file filename))
                        (http/stop!)
                        (load-file (absolute-path (str path "/reptile/server/socket_repl.clj")))
                        (load-file (absolute-path (str path "/reptile/server/http.clj")))
                        (start-server port secret socket-host socket-port)))
       :options     {:recursive true}}])))
