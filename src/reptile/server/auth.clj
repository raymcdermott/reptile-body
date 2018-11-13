(ns reptile.server.auth
  (:require
    [aleph.http :as http]
    [clojure.data.json :as json]
    [clojure.java.io :refer [file]]
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn parse
  [response]
  (if-let [body (:body response)]
    (let [content-type (get-in response [:headers "content-type"])
          json?        (and content-type (str/starts-with? content-type "application/json"))]
      (assoc response :body (if json?
                              (json/read (io/reader body) :key-fn keyword)
                              (slurp body))))
    response))

(defn get-oauth-token
  []
  (let [body     {:client_id     "G7av5Uo3JJv4fkwS3aHQzxqe7CiB9Eut"
                  :client_secret "1AdasMFfe-kuCw0E2mVfGrXSMUcZ8PHau1wsRaU3iKwqpWXrcohUqLPZzoDy6jxw"
                  :audience      "https://extemporary.auth0.com/api/v2/"
                  :grant_type    "client_credentials"}
        response (parse
                   @(http/post
                      "https://extemporary.auth0.com/oauth/token"
                      {:headers          {"content-type" "application/json"}
                       :body             (json/write-str body)
                       :throw-exceptions false}))]
    (if (= 200 (:status response))
      (-> response :body :access_token)
      (throw (ex-info "Failed to get OAUTH token." {:response response})))))

(defn get-apitoken
  [oauth-token username]
  (let [response (parse
                   @(http/get
                      (str "https://extemporary.auth0.com/api/v2/users/" username)
                      {:headers          {"authorization" (str "Bearer " oauth-token)}
                       :throw-exceptions false}))]
    (if (= 200 (:status response))
      (-> response :body :identities first :access_token)
      (throw (ex-info "Failed to get API token." {:response response})))))

(defn api-token
  []
  (let [token (get-oauth-token)]
    (get-apitoken token "github|120437")))

(defn gist-data
  [content]
  {:description "REPtiLe Evaluation History"
   :files       {:repfile
                 {:filename "reptile-session.clj"
                  :content  content}}})

(defn send-gist
  [gist api-token]
  (let [headers  {"Authorization" (str "token " api-token)
                  "User-Agent"    "reptile-ui.extemporay.io"
                  "Accept"        "application/vnd.github.v3+json"}
        response (parse
                   @(http/post
                      "https://api.github.com/gists"
                      {:headers          headers
                       :body             (json/write-str gist)
                       :throw-exceptions false}))]
    (if (= 201 (:status response))
      (-> response :body :html_url)
      (throw (ex-info "Failed to post GIST." {:response response})))))

(defn test-send
  []
  (let [gist  (gist-data "(+ 1 2)")
        token (api-token)]
    (send-gist gist token)))



