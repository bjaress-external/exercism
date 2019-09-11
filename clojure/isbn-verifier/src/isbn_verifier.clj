(ns isbn-verifier)

(defn isbn-sum [digits]
  (defn value-at [index digit]
    (def value (if (= "X" digit) 10 (bigint digit)))
    (* (- 10 index) value))
  (mod (apply + (keep-indexed value-at digits)) 11))

(defn isbn? [isbn]
  (if (nil? (re-find #"^([0-9]-*){9}[0-9X]$" isbn))
    false
    (= 0 (isbn-sum (map (fn [[_ digit]] digit)
                        (re-seq #"([0-9X])-*" isbn))))))
