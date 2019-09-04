(ns rna-transcription
  (:require [clojure.string :as string]))

(def complement {
                 \G \C
                 \C \G
                 \T \A
                 \A \U
                 })

(defn to-rna [dna]
  (defn checked-complement [base]
    (or (complement base) (assert false)))
  (string/join (map checked-complement (seq dna))))
