(ns anagram
  (:require [clojure.string :as str]))

(defn normalize [word]
  (let [lower (str/lower-case word)
        letters (frequencies lower)]
    [lower letters]))

(defn match [[target-word target-letters] [prospect-word prospect-letters]]
  (and (= target-letters prospect-letters)
       (not (= target-word prospect-word))))

(defn anagrams-for [word prospect-list]
  (filter (comp (partial match (normalize word))
                normalize)
          prospect-list))
