(ns advent.2024.7
  (:require
    [clojure.string :as s]))

;; IO

(defn parse-line [input]
  (let [[_ test-val-str remaining-string] (re-matches #"(\d+): (.*)" input)]
    [(parse-long test-val-str) (mapv parse-long (re-seq #"\d+" remaining-string))]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def input-file "sample.dat")
(def input-data (->> input-file slurp parse-input))

;; Part 1

(defn eval-equation [numbers ops]
  (if (= 1 (count numbers))
    (first numbers)
    (let [args (take 2 numbers)
          op (first ops)
          result (apply op args)]
      (recur
        (conj (drop 2 numbers) result)
        (rest ops)))))

(defn next-ops [ops]
  (if (seq ops)
    (if (= + (first ops))
      (conj (drop 1 ops) *)
      (conj (next-ops (drop 1 ops)) +))
    '()))

(defn valid-equation? [[test-val numbers]]
  (let [num-count (count numbers)
        initial-ops (repeat (dec num-count) +)
        num-combinations (int (Math/pow 2 (count initial-ops)))]
    (loop [ops initial-ops
           i 0]
      (if (< i num-combinations)
        (if (= test-val (eval-equation numbers ops))
          true
          (recur (next-ops ops) (inc i)))
        false))))

(defn solve [input]
  (->> input
    (filter valid-equation?)
    (map first)
    (apply +)))

(solve input-data)

;; Part 2

(defn join [x y]
  (parse-long (str x y)))

(defn next-ops-2 [ops]
  (if (seq ops)
    (cond
      (= + (first ops)) (conj (drop 1 ops) *)
      (= * (first ops)) (conj (drop 1 ops) join)
      :else (conj (next-ops-2 (drop 1 ops)) +))
    '()))

(defn valid-equation-2? [[test-val numbers]]
  (let [num-count (count numbers)
        initial-ops (repeat (dec num-count) +)
        num-combinations (int (Math/pow 3 (count initial-ops)))]
    (loop [ops initial-ops
           i 0]
      (if (< i num-combinations)
        (if (= test-val (eval-equation numbers ops))
          true
          (recur (next-ops-2 ops) (inc i)))
        false))))

(defn solve-2 [input]
  (->> input
    (filter valid-equation-2?)
    (map first)
    (apply +)))

(solve-2 input-data)
