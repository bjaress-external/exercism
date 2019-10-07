(ns say
  (:require
    [clojure.string :as str]
    [clojure.math.numeric-tower :as numeric]))

(def very-small
  [ "zero"
    "one"
    "two"
    "three"
    "four"
    "five"
    "six"
    "seven"
    "eight"
    "nine"
    "ten"
    "eleven"
    "twelve"
    "thirteen"
    "fourteen"
    "fifteen"
    "sixteen"
    "seventeen"
    "eighteen"
    "nineteen"
    ])

(def tens
  [ "zero"
    "ten"
    "twenty"
    "thirty"
    "forty"
    "fifty"
    "sixty"
    "seventy"
    "eighty"
    "ninety"
    ])

(defn handle-parts [num divisor handle-upper separator handle-lower]
  (let [upper (quot num divisor)
        lower (rem num divisor)]
    [(handle-upper upper) separator (handle-lower lower)]))

(defn zeros [count] (numeric/expt 10 count))

(defn say [num]
  (cond
    (< num 0) (throw (IllegalArgumentException. "Too small!"))
    (< num (count very-small)) (very-small num)
    (< num 100) (str/join (handle-parts num 10 tens "-" very-small))
    (< num (zeros 3)) (str/join " " (handle-parts num (zeros 2) say "hundred" say))
    (< num (zeros 6)) (str/join " " (handle-parts num (zeros 3) say "thousand" say))
    (< num (zeros 9)) (str/join " " (handle-parts num (zeros 6) say "million" say))
    (< num (zeros 12)) (str/join " " (handle-parts num (zeros 9) say "billion" say))
    :else (throw (IllegalArgumentException. "Too large!"))))

(defn number [num]
  (str/replace (say num) #".zero$" ""))
