(ns advent.2024.25
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-schematic [input]
  (map vec (s/split-lines input)))

(defn parse-input [input]
    (for [schematic (s/split input #"\n\n")]
      (vec (parse-schematic schematic))))

(def input-file "input.dat")

(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(defn count-height [grid]
  (->> grid
       (u/transpose)
       (map (fn [row] (filter #(= \# %) row)))
       (mapv count)
       (mapv dec)))

(defn to-heights [input]
  (loop [xs input
         keys #{}
         locks #{}]
    (if (seq xs)
      (let [item (first xs)]
        (if (= [\# \# \# \# \#] (first item))
          (recur (rest xs) keys (conj locks (count-height item)))
          (recur (rest xs) (conj keys (count-height item)) locks)))
      [keys locks])))

(defn fits? [key lock]
  (->> (u/vector+ key lock)
       (filter #(< 5 %))
       (empty?)))

(defn solve [raw-input]
  (let [[keys locks] (to-heights raw-input)]
    (apply +
      (for [k keys l locks]
        (if (fits? k l) 1 0)))))

(solve input-data)
