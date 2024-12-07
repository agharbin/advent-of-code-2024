(ns advent.2024.6
  (:require
    [clojure.string :as s]
    [clojure.set :as set]
    [util :as u]))

;; IO

(defn parse-line [input]
  (vec input))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def input-file "input.dat")
(def input-grid (->> (slurp input-file) parse-input))
(def num-rows (count input-grid))
(def num-cols (count (first input-grid)))
(def start-position (first
                      (filter #(#{\^ \v \< \>} (get-in input-grid %))
                              (for [r (range num-rows) c (range num-cols)] [r c]))))
(def start-facing [-1 0])

;; Part 1

(defn inside-grid? [grid position]
  (some? (get-in grid position nil)))

(def turn { [ 1  0] [ 0 -1], [ 0 -1] [-1  0], [-1  0] [ 0  1], [ 0  1] [ 1  0] })

(defn advance-position-and-facing [grid [position facing]]
  (let [position-ahead (u/vector+ position facing)
        position-ahead-contents (get-in grid position-ahead)]
    (if (= \# position-ahead-contents)
      [position (turn facing)]
      [position-ahead facing])))

(defn find-visited-positions [grid]
  (loop [[position facing] [start-position start-facing]
         visited #{}]
    (if (inside-grid? grid position)
      (recur (advance-position-and-facing grid [position facing]) (conj visited position))
      visited)))

(defn solve [grid]
  (count (find-visited-positions grid)))

(solve input-grid)

;; Part 2

(defn path-loops? [grid]
  (loop [[position facing] [start-position start-facing]
         visited #{}]
    (cond
      (not (inside-grid? grid position)) false
      (visited [position facing]) true
      :else (recur
              (advance-position-and-facing grid [position facing])
              (conj visited [position facing])))))

(defn solve-2 [grid]
  (let [candidates (set/difference (find-visited-positions grid) #{start-position})]
    (count
      (filter #(path-loops? (assoc-in grid % \#)) candidates))))

(solve-2 input-grid)
