(ns anagram
  (:require [clojure.string :as str]))

(defn match [target prospect]
  (let [t (str/lower-case target)
        p (str/lower-case prospect)]
    (and (= (frequencies t) (frequencies p))
         (not (= t p)))))

(defn anagrams-for [word prospect-list]
  (filter (partial match word) prospect-list))
