(ns run-length-encoding)

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (with-out-str
    (doseq [chunk (partition-by identity plain-text)]
      (if (> (count chunk) 1)
        (print (count chunk)))
      (print (first chunk)))))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (with-out-str
    (doseq [[_ prefix letter] (re-seq #"([0-9]*)([^0-9])" cipher-text)]
      (def repeats
        (if (= "" prefix)
          1
          (bigint prefix)))
      (dotimes [_ repeats] (print letter)))))
