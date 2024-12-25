(ns advent.2024.21.2
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-line [input]
  (let [[code numeric-part-str] (re-matches #"(\d+)A" input)]
    [code (parse-long numeric-part-str)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def input-file "input.dat")

(def input-data (->> (slurp input-file) parse-input))

;; Part 2

(def keypad [[ \7 \8 \9]
             [ \4 \5 \6]
             [ \1 \2 \3]
             [nil \0 \A]])

(def dpad [[nil \^ \A]
           [ \< \v \>]])

(def keypad-button->position
  (->> (for [r (range (count keypad)) c (range (count (first keypad)))]
         [(get-in keypad [r c]) [r c]])
       (filter (fn [[k v]] (some? k)))
       (into {})))

(def dpad-button->position
  (->> (for [r (range (count dpad)) c (range (count (first dpad)))]
         [(get-in dpad [r c]) [r c]])
       (filter (fn [[k v]] (some? k)))
       (into {})))

(def button->offset
  {\> [0 1]
   \< [0 -1]
   \^ [-1 0]
   \v [1 0]})

(defn possible-paths [pad [r1 c1] [r2 c2]]
  (let [n-right-moves (max 0 (- c2 c1))
        n-left-moves (max 0 (- c1 c2))
        n-down-moves (max 0 (- r2 r1))
        n-up-moves (max 0 (- r1 r2))]
    (->>
      (concat (repeat n-right-moves \>)
              (repeat n-left-moves \<)
              (repeat n-up-moves \^)
              (repeat n-down-moves \v))
      combo/permutations
      (filter (fn [moves] 
                (->> moves
                     (map button->offset)
                     (reductions u/vector+ [r1 c1])
                     (map #(get-in pad %))
                     (every? some?))))
      (map vec)
      (map #(conj % \A)))))

(declare compute-shortest-dpad-path-length)

(def find-shortest-dpad-path-between-buttons
  (memoize
    (fn [level button-1 button-2]
      (let [paths (possible-paths dpad (dpad-button->position button-1) (dpad-button->position button-2))]
        (if (zero? level)
          (->> paths (map count) (apply min))
          (->> (for [path paths] (compute-shortest-dpad-path-length (dec level) path))
               (apply min)))))))

(def compute-shortest-dpad-path-length
  (memoize
    (fn [level path]
      (let [pairs (->> (conj (seq path) \A) (partition 2 1))]
        (apply +
          (for [[button-1 button-2] pairs]
            (find-shortest-dpad-path-between-buttons level button-1 button-2)))))))

(def find-shortest-keypad-path-between-buttons
  (memoize
    (fn [level button-1 button-2]
      (let [paths (possible-paths keypad (keypad-button->position button-1) (keypad-button->position button-2))]
      (if (zero? level)
        (->> paths (map count) (apply min))
        (->> (for [path paths] (compute-shortest-dpad-path-length (dec level) path))
             (apply min)))))))

(def compute-shortest-keypad-path-length
  (memoize
    (fn [level path]
      (let [pairs (->> (conj (seq path) \A) (partition 2 1))]
        (apply +
          (for [[button-1 button-2] pairs]
            (find-shortest-keypad-path-between-buttons level button-1 button-2)))))))

(defn compute-score [level input]
  (apply +
    (for [[code value] input]
      (* value (compute-shortest-keypad-path-length level code)))))

(compute-score 25 input-data)
