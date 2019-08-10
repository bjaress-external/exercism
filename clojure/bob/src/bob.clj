(ns bob)

(defn lower? [s]
  (re-find #"[a-z]" s))

(defn upper? [s]
  (re-find #"[A-Z]" s))

(defn shout? [s]
  (and (upper? s) (not (lower? s))))

(defn question? [s]
  (re-matches #".*\?$" s))

(defn nothing? [s]
  (re-matches #"\W*" s))

(defn response-for [s]
  (cond
    (and (shout? s) (question? s)) "Calm down, I know what I'm doing!"
    (shout? s) "Whoa, chill out!"
    (question? s) "Sure."
    (nothing? s) "Fine. Be that way!"
    :else "Whatever."))
