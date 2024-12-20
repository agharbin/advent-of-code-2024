(ns advent.2024.19
  (:require
    [clojure.string :as s]))

;; IO

(defn parse-input [input]
  (let [[towels-str designs-str] (s/split input #"\n\n")
        towels (s/split towels-str #", ")
        designs (s/split designs-str #"\n")]
    [towels designs]))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))
(def towels (first input-data))
(def designs (second input-data))

;; Part 1

(defn solve []
  (let [p (re-pattern (str "(" (s/join "|" towels) ")*"))]
    (->> (for [d designs] (re-matches p d))
         (filter some?)
         count)))

(solve)

;; Part 2

(defn is-prefix? [towel design]
  (let [len (count towel)]
    (if (< (count design) len)
      false
      (= towel (subs design 0 len)))))

(defn drop-towel [towel design]
  (apply str (drop (count towel) design)))

(def count-combinations
  (memoize
    (fn [design]
      (if (zero? (count design))
        1
        (->> (for [t towels] (if (is-prefix? t design) (count-combinations (drop-towel t design)) 0))
             (apply +))))))

(defn solve-2 []
  (apply + (map count-combinations designs)))

(solve-2)
