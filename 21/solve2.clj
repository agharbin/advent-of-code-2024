(ns advent.2024.21.2
  (:require
    [clojure.data.priority-map :as pm]
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

;; Part 1

(def keypad [[ \7  \8 \9]
             [ \4  \5 \6]
             [ \1  \2 \3]
             [nil  \0 \A]])

(def d-pad [[nil \^ \A]
            [ \< \v \>]])

(def keypad-button->position
  (->> (for [r (range (count keypad)) c (range (count (first keypad)))]
         [(get-in keypad [r c]) [r c]])
       (filter (fn [[k v]] (some? k)))
       (into {})))

(def all-keypad-buttons (keys keypad-button->position))

(def d-pad-button->position
  (->> (for [r (range (count d-pad)) c (range (count (first d-pad)))]
         [(get-in d-pad [r c]) [r c]])
       (filter (fn [[k v]] (some? k)))
       (into {})))

(def all-d-pad-buttons (keys d-pad-button->position))

(defn manhattan-distance [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2))
     (abs (- c1 c2))))

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

(defn shortest-d-pad-path [exploration-level button-1 button-2]
  (if (zero? exploration-level)
    (->> (possible-paths d-pad (d-pad-button->position button-1) (d-pad-button->position button-2))
         (sort-by count)
         first)
    (->> (for [p (possible-paths d-pad (d-pad-button->position button-1) (d-pad-button->position button-2))]
           [p
            (apply concat
             (for [[b1 b2] (->> (conj (seq p) \A) (partition 2 1))]
               (shortest-d-pad-path (dec exploration-level) b1 b2)))])
         (sort-by #(count (second %)))
         first
         first)))

(def shortest-d-pad-paths
  (->>
    (for [b1 all-d-pad-buttons
          b2 all-d-pad-buttons]
      [[b1 b2] (shortest-d-pad-path 2 b1 b2)])
    (into {})))

(defn shortest-keypad-path [exploration-level button-1 button-2]
  (if (zero? exploration-level)
    (->> (possible-paths d-pad (d-pad-button->position button-1) (d-pad-button->position button-2))
         (sort-by count)
         first)
    (->> (for [p (possible-paths keypad (keypad-button->position button-1) (keypad-button->position button-2))]
           [p
            (apply concat
             (for [[b1 b2] (->> (conj (seq p) \A) (partition 2 1))]
               (shortest-d-pad-path (dec exploration-level) b1 b2)))])
         (sort-by #(count (second %)))
         first
         first)))

(def shortest-keypad-paths
  (->>
    (for [b1 all-keypad-buttons
          b2 all-keypad-buttons]
      [[b1 b2] (shortest-keypad-path 3 b1 b2)])
    (into {})))

(declare compute-d-pad-path-length)

(def compute-d-pad-distance
  (memoize
    (fn [level button-1 button-2]
      (compute-d-pad-path-length level (shortest-d-pad-paths [button-1 button-2])))))

(def compute-d-pad-path-length
  (memoize
    (fn [level path]
      (if (zero? level)
        (count path)
        (->> (conj (seq path) \A)
             (partition 2 1)
             (map (fn [[b1 b2]] (compute-d-pad-distance (dec level) b1 b2)))
             (apply +))))))

(defn compute-keypad-distance [num-d-pads button-1 button-2]
  (compute-d-pad-path-length num-d-pads (shortest-keypad-paths [button-1 button-2])))

(defn compute-keypad-path-length [num-d-pads path]
  (->> (conj (seq path) \A)
       (partition 2 1)
       (map (fn [[b1 b2]] (compute-keypad-distance num-d-pads b1 b2)))
       (apply +)))

(defn solve-2 [input num-d-pads]
  (apply +
    (for [[code value] input]
      (* (compute-keypad-path-length num-d-pads code) value))))

(solve-2 input-data 2)
