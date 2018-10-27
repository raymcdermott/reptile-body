(ns reptile.dev.reload-server
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
  [& [port]]
  (letfn [(start-reptile [port]
            (start-server (or port 56665) "warm-blooded-lizards-rock"))]
    (start-watch [{:path        (absolute-path "src")
                   :event-types [:create :modify :delete]
                   :bootstrap   (fn [path]
                                  (println "Starting to watch " path)
                                  (start-reptile port))
                   :callback    (fn [event filename]
                                  (println event filename)
                                  (load-file (absolute-path "src/reptile/server/http.clj"))
                                  (http/stop-reptile-server)
                                  (start-reptile port))
                   :options     {:recursive true}}])))


;------------------- ************* -------------------
;
; Start up when the namespace is loaded
;
;------------------- ************* -------------------

(boot-and-watch-fs!)
