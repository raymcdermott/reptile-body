(ns reptile.server.completion
  (:require [compliment.core :as compliment]))

; TODO limit type to function for now

; TODO learn the lib in detail

(defn code-completions
  [prefix-token form-with-prefix]
  (when prefix-token
    (compliment/completions prefix-token {:context form-with-prefix})))