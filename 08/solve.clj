(ns advent.2024.8
  (:require
    [clojure.string :as s]
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

;; Part 1

(defn find-antennas [grid]
  (let [num-rows (count grid)
        num-cols (count (first grid))]
    (loop [xs (for [r (range num-rows) c (range num-cols)] [r c])
           result {}]
      (if (seq xs)
        (let [location (first xs)
              symbol (get-in grid location)]
          (if (= \. symbol)
            (recur (rest xs) result)
            (if (not (result symbol))
              (recur (rest xs) (assoc result symbol #{location}))
              (recur (rest xs) (update result symbol #(conj % location))))))
        result))))

(defn find-antinodes-for-type [antennas]
  (loop [xs (for [i antennas j antennas :when (not= i j)] [i j])
         antinodes #{}]
    (if (seq xs)
      (let [[x y] (first xs)
            distance (map #(apply - %) (u/transpose [x y]))
            twice-distance (map #(* 2 %) distance)
            antinode (u/vector+ y twice-distance)]
        (recur (rest xs) (conj antinodes antinode)))
      antinodes)))

(defn solve [antinode-find-fn grid]
  (let [antennas (find-antennas grid)]
    (loop [xs antennas
           all-antinodes #{}]
      (if (seq xs)
        (recur
          (rest xs)
          (into all-antinodes
                (filter #(get-in grid %)
                        (antinode-find-fn (second (first xs))))))
        (count all-antinodes)))))

(solve find-antinodes-for-type input-grid)

;; Part 2

(defn find-antinodes-for-type-2 [antennas]
  (loop [xs (for [i antennas j antennas :when (not= i j)] [i j])
         antinodes #{}]
    (if (seq xs)
      (let [[x y] (first xs)
            distance (map #(apply - %) (u/transpose [x y]))
            indexes (range 50)
            new-antinodes (for [i indexes] (u/vector+ y (map #(* i %) distance)))]
        (recur (rest xs) (into antinodes new-antinodes)))
      antinodes)))

(solve find-antinodes-for-type-2 input-grid)
