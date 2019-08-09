(ns reverse-string
  (:require [clojure.string :as string]))

(defn reverse-string [s]
  (loop [forward s backward '()]
    (if (empty? forward)
      (string/join backward)
      (recur (rest forward) (cons (first forward) backward)))))
