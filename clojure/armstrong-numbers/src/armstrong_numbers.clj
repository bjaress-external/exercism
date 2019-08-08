(ns armstrong-numbers
  (:require [clojure.math.numeric-tower :as numeric]))

(defn armstrong? [num] ;; <- arglist goes here

  ; Take quotient and remainder, divide quotient by 10 to get new
  ; quotient and remainder
  (defn divide-step [previous]
    (let [{prev :quot} previous]
      {:quot (quot prev 10)
       :rem (rem prev 10)}))

  (def divisions
    ; Keep dividing until everything goes to 0
    (take-while (fn [{q :quot r :rem}] (or (pos? q) (pos? r)))
                (rest (iterate divide-step {:quot num :rem 0} ))))

  (def digits (map :rem divisions))
  (def digit-count (count digits))

  (def total
    ; Take each digit to the power of the number of digits and then sum
    (apply + (map (fn [digit] (numeric/expt digit digit-count)) digits)))

  (= total num)
)
