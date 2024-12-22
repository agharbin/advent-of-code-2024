(ns advent.2024.22
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-long)))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(def mod-val 16777216)

(defn next-number [n]
  (let [result n
        result (mod (bit-xor result (* 64 result)) mod-val)
        result (mod (bit-xor result (quot result 32)) mod-val)
        result (mod (bit-xor result (* 2048 result)) mod-val)]
    result))

(defn solve [input]
  (->> input
    (map #(first (drop 2000 (iterate next-number %))))
    (apply +)))

(solve input-data)

;; Part 2

(defn prices [n]
  (->> (iterate next-number n)
       (map #(mod % 10))))

(defn changes [xs]
  (->> xs
       (partition 2 1)
       (map #(* -1 (apply - %)))))

(defn build-sequence-map [n]
  (loop [xs (->> (u/zip (drop 1 (prices n)) (changes (prices n)))
                 (partition 4 1)
                 (take 1999))
         seq->price {}]
    (if (seq xs)
      (let [change-seq-and-prices (first xs)
            change-seq (map second change-seq-and-prices)
            price (first (last change-seq-and-prices))]
        (if (seq->price change-seq)
          (recur (rest xs) seq->price)
          (recur (rest xs) (conj seq->price [change-seq price]))))
      seq->price)))

(defn solve-2 [input-data]
  (let [maps (map build-sequence-map input-data)]
    (apply max
      (for [i (range -9 9)
            j (range -9 9)
            k (range -9 9)
            l (range -9 9)]
        (apply + (map #(get % [i j k l] 0) maps))))))

(solve-2 input-data)
