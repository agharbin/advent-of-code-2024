(ns advent.2024.21.1
  (:require
    [clojure.data.priority-map :as pm]
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

(defn push-a [level [[keypad-position d-pad-positions] cost]]
  (if (= level (dec (count d-pad-positions)))
    (let [button-state (get-in d-pad (d-pad-positions level))
          next-button-position keypad-position
          next-cost (inc cost)]
      (case button-state
        \^ (if (some? (get-in keypad (u/vector+ next-button-position [-1 0])))
             [[[(u/vector+ next-button-position [-1 0]) d-pad-positions] next-cost]]
             [])
        \v (if (some? (get-in keypad (u/vector+ next-button-position [1 0])))
             [[[(u/vector+ next-button-position [1 0]) d-pad-positions] next-cost]]
             [])
        \< (if (some? (get-in keypad (u/vector+ next-button-position [0 -1])))
             [[[(u/vector+ next-button-position [0 -1]) d-pad-positions] next-cost]]
             [])
        \> (if (some? (get-in keypad (u/vector+ next-button-position [0 1])))
             [[[(u/vector+ next-button-position [0 1]) d-pad-positions] next-cost]]
             [])
        \A []))
    (let [button-state (get-in d-pad (d-pad-positions level))
          next-button-position (d-pad-positions (inc level))
          next-cost (inc cost)]
      (case button-state
        \^ (if (some? (get-in d-pad (u/vector+ next-button-position [-1 0])))
             [[[keypad-position (assoc d-pad-positions (inc level) (u/vector+ next-button-position [-1 0]))]
               next-cost]]
             [])
        \v (if (some? (get-in d-pad (u/vector+ next-button-position [1 0])))
             [[[keypad-position (assoc d-pad-positions (inc level) (u/vector+ next-button-position [1 0]))]
               next-cost]]
             [])
        \< (if (some? (get-in d-pad (u/vector+ next-button-position [0 -1])))
             [[[keypad-position (assoc d-pad-positions (inc level) (u/vector+ next-button-position [0 -1]))]
               next-cost]]
             [])
        \> (if (some? (get-in d-pad (u/vector+ next-button-position [0 1])))
             [[[keypad-position (assoc d-pad-positions (inc level) (u/vector+ next-button-position [0 1]))]
               next-cost]]
             [])
        \A (push-a (inc level) [[keypad-position d-pad-positions] cost])))))

(def all-d-pad-positions (->> (for [r (range (count d-pad)) c (range (count (first d-pad)))] [r c])
                          (filter #(some? (get-in d-pad %)))
                          (into #{})))

(defn next-states [[[keypad-position d-pad-positions] cost]]
  (let [d1-position (first d-pad-positions)
        legal-d1-moves (->> (u/four-neighbors d1-position)
                            (filter all-d-pad-positions))]
      (into
        (for [move legal-d1-moves] [[keypad-position (assoc d-pad-positions 0 move)] (inc cost)])
        (push-a 0 [[keypad-position d-pad-positions] cost]))))

(defn run-dijkstra [[keypad-start d-pad-positions :as start-state] goal-positions]
  (if (= start-state goal-positions)
    0
    (loop [q (conj (pm/priority-map) [start-state 0])
           positions->distance {}]
      (let [[positions cost :as state] (peek q)]
        (if (= goal-positions positions)
          (positions->distance goal-positions)
          (let [candidates (next-states state)
                better-candidates (filter (fn [[new-positions new-score]]
                                            (< new-score (get positions->distance new-positions Integer/MAX_VALUE)))
                                          candidates)]
            (recur
              (into (pop q) better-candidates)
              (into positions->distance better-candidates))))))))

(def button->d-pad-position
  (->> (for [r (range (count d-pad)) c (range (count (first d-pad)))] [r c])
       (filter #(some? (get-in d-pad %)))
       (map #(vector (get-in d-pad %) %))
       (into {})))

(def button->keypad-position
  (->> (for [r (range (count keypad)) c (range (count (first keypad)))] [r c])
       (filter #(some? (get-in keypad %)))
       (map #(vector (get-in keypad %) %))
       (into {})))

(defn keypad-positions [button num-d-pads]
  [(button->keypad-position button)
   (vec (repeat num-d-pads (button->d-pad-position \A)))])

(defn code-cost [code num-d-pads]
  (let [[a b c d] (seq code)]
    (+
      (run-dijkstra (keypad-positions \A num-d-pads) (keypad-positions a num-d-pads))
      (run-dijkstra (keypad-positions a num-d-pads) (keypad-positions b num-d-pads))
      (run-dijkstra (keypad-positions b num-d-pads) (keypad-positions c num-d-pads))
      (run-dijkstra (keypad-positions c num-d-pads) (keypad-positions d num-d-pads))
      4)))

(defn solve [data num-d-pads]
  (apply +
    (for [[code numeric-val] data]
      (* (code-cost code num-d-pads) numeric-val))))

(time (solve input-data 2))
