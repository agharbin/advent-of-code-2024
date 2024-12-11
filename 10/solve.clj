(ns advent.2024.10
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-line [input]
  (mapv #(parse-long (str %)) (vec input)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def input-filename "input.dat")
(def input-grid (->> (slurp input-filename) parse-input))

;; Part 1

(defn find-positions-at-height [grid height]
  (let [rows (count grid)
        cols (count (first grid))]
    (->> (for [r (range rows) c (range cols)] [r c])
         (filter #(= height (get-in grid %))))))

(defn find-neighbors [grid location]
  (->> (for [offset [[1 0] [-1 0] [0 1] [0 -1]]] (u/vector+ location offset))
       (filter #(get-in grid %))))

(defn find-peak-positions [grid trailhead-position]
  (loop [search-space [trailhead-position]
         peak-locations-found []]
    (if (seq search-space)
      (let [curr-position (first search-space)
            curr-height (get-in grid curr-position)]
        (if (= 9 curr-height)
          (recur (rest search-space) (conj peak-locations-found curr-position))
          (recur
            (into (rest search-space)
                  (->> (find-neighbors grid curr-position)
                       (filter #(= (inc curr-height) (get-in grid %)))))
            peak-locations-found)))
      peak-locations-found)))

(defn solve-1 [grid]
  (apply +
    (for [loc (find-positions-at-height grid 0)]
      (->> (find-peak-positions grid loc)
           (into #{})
           count))))

(solve-1 input-grid)

;; Part 2

(defn solve-1 [grid]
  (apply +
    (for [loc (find-positions-at-height grid 0)]
      (->> (find-peak-positions grid loc)
           count))))

(solve-2 input-grid)
