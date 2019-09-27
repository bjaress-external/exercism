(ns anagram
  (:require [clojure.string :as str]))

(defn letter-bag [word]
  (apply merge-with +
         (map (fn [letter] {letter 1}) word)))

(defn match [target prospect]
  (let [t (str/lower-case target)
        p (str/lower-case prospect)]
    (and (= (letter-bag t) (letter-bag p))
         (not (= t p)))))

(defn anagrams-for [word prospect-list]
  (filter (partial match word) prospect-list))
