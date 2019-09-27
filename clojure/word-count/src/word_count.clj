(ns word-count
  (:require [clojure.string :as str]))

(defn word-count [s]
  (def words
    (filter (comp not empty?)
            (str/split (str/lower-case s) #"[^a-z0-9]")))
  (apply merge-with +
         (map (fn [word] {word 1}) words)))
