(ns advent.2024.15
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-grid [grid-str]
  (mapv vec (s/split-lines grid-str)))

(defn parse-moves [moves-str]
  (apply concat (mapv vec (s/split-lines moves-str))))

(defn parse-input [input]
  (let [[grid-str moves-str] (s/split input #"\n\n")]
    [(parse-grid grid-str) (parse-moves moves-str)]))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(defn get-offset [direction]
  (case direction \^ [-1 0] \v [1 0] \< [0 -1] \> [0 1]))

(defn find-empty-space [grid position direction]
  (loop [p position]
    (let [offset (get-offset direction)
          check-position (u/vector+ p offset)
          contents (get-in grid check-position)]
      (cond
        (= \# contents) nil
        (= \. contents) check-position
        :else (recur (u/vector+ p offset))))))

(defn simulate-move [[grid position] direction]
  (let [[pr pc] position
        empty-space (find-empty-space grid position direction)]
    (if empty-space
      (let [[sr sc] empty-space
            min-row (min pr sr)
            max-row (inc (max pr sr))
            min-col (min pc sc)
            max-col (inc (max pc sc))
            boxes-and-robot (filter #(not= \. (get-in grid %))
                              (for [r (range min-row max-row) c (range min-col max-col)] [r c]))
            offset (get-offset direction)]
        [(assoc-in
           (reduce (fn [m p] (assoc-in m (u/vector+ p offset) (get-in grid p))) grid boxes-and-robot)
           position
           \.)
         (u/vector+ position offset)])
      [grid position])))

(defn compute-score [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (apply + (for [r (range rows) c (range cols)] (if (= \O (get-in grid [r c])) (+ (* r 100) c) 0)))))

(defn solve [[grid moves]]
  (let [rows (count grid)
        cols (count (first grid))
        initial-position (first (filter #(= (get-in grid %) \@) (for [r (range rows) c (range cols)] [r c])))]
    (compute-score
      (first (reduce simulate-move [grid initial-position] moves)))))

(solve input-data)

;; Part 2

(defn substitute [c]
  (case c
    \# [\# \#]
    \O [\[ \]]
    \. [\. \.]
    \@ [\@ \.]))

(defn expand-row [row]
  (vec (apply concat (map substitute row))))

(defn expand-grid [grid]
  (mapv expand-row grid))

(def expanded-grid (expand-grid (first input-data)))
(def moves (second input-data))

(defn find-moveable-locations [grid position direction]
  (let [offset (get-offset direction)
        forward-position (u/vector+ position offset)
        forward-contents (get-in grid forward-position)]
    (cond
      (= \# forward-contents) nil
      (= \. forward-contents) #{position}
      (#{\[ \]} forward-contents)
        (if (#{ \< \>} direction)
          (let [forward-able-to-move (find-moveable-locations grid forward-position direction)]
            (if (some? forward-able-to-move)
              (conj forward-able-to-move position)
              nil))
          (let [this-side-locations (find-moveable-locations grid forward-position direction)
                neighbor (if (= forward-contents \[)
                           (u/vector+ forward-position [0 1])
                           (u/vector+ forward-position [0 -1]))
                neighbor-locations (find-moveable-locations grid neighbor direction)]
            (if (and (some? this-side-locations) (some? neighbor-locations))
              (conj (into this-side-locations neighbor-locations) position)
              nil)))
      :else (assert false))))

(defn simulate-move-2 [[grid position] direction]
  (let [offset (get-offset direction)
        locations-able-to-move (find-moveable-locations grid position direction)]
    (if (some? locations-able-to-move)
      (let [sorted (case direction
                     \^ (sort-by first locations-able-to-move)
                     \v (reverse (sort-by first locations-able-to-move))
                     \< (sort-by second locations-able-to-move)
                     \> (reverse (sort-by second locations-able-to-move)))]
        ; Swap locations of boxes with whatever is in front of them sorted from
        ; front to back.
        [(reduce (fn [m p] (-> m
                               (assoc-in (u/vector+ p offset) (get-in m p))
                               (assoc-in p (get-in m (u/vector+ p offset)))))
                 grid
                 sorted)
         (u/vector+ position offset)])
      [grid position])))

(defn compute-score-2 [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (apply + (for [r (range rows) c (range cols)] (if (= \[ (get-in grid [r c])) (+ (* r 100) c) 0)))))

(defn solve-2 [[grid moves]]
  (let [rows (count grid)
        cols (count (first grid))
        initial-position (first (filter #(= (get-in grid %) \@) (for [r (range rows) c (range cols)] [r c])))]
    (compute-score-2
      (first (reduce simulate-move-2 [grid initial-position] moves)))))

(solve-2 [expanded-grid moves])
