(ns advent.2024.14
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-line [input]
  (let [[_ px py vx vy] (re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" input)]
    [(mapv parse-long [px py])
     (mapv parse-long [vx vy])]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

(def x-dimension 101)
(def y-dimension 103)
(def iterations 100)

;; Part 1

(defn advance-position [[start-position velocity]]
  (let [[x y] (u/vector+ start-position velocity)]
    [[(mod x x-dimension) (mod y y-dimension)]
     velocity]))

(defn divide-into-quadrants [robots]
  (let [x-center (int (/ x-dimension 2))
        y-center (int (/ y-dimension 2))]
    (loop [xs robots
           q1 0 q2 0 q3 0 q4 0]
      (if (seq xs)
        (let [[x y] (first xs)]
          (cond
            (and (< x x-center) (< y y-center)) (recur (rest xs) (inc q1) q2 q3 q4)
            (and (> x x-center) (< y y-center)) (recur (rest xs) q1 (inc q2) q3 q4)
            (and (< x x-center) (> y y-center)) (recur (rest xs) q1 q2 (inc q3) q4)
            (and (> x x-center) (> y y-center)) (recur (rest xs) q1 q2 q3 (inc q4))
            :else (recur (rest xs) q1 q2 q3 q4)))
        [q1 q2 q3 q4]))))

(defn solve [robots]
  (loop [xs robots
         i 0]
    (if (< i iterations)
      (recur (map advance-position xs) (inc i))
      (apply * (divide-into-quadrants (map first xs))))))

(solve input-data)

;; Part 2

(defn print-robots [positions]
  (let [blank-space (vec (for [y (range y-dimension)]
                      (vec (for [x (range x-dimension)] \.))))
        symbols (reduce (fn [grid [x y]] (assoc-in grid [y x] \X)) blank-space positions)]
    (loop [row 0]
      (if (< row y-dimension)
        (do
          (assert (= x-dimension (count (apply str (symbols row)))))
          (print (str (apply str (symbols row)) \newline))
          (recur (inc row)))
        nil))))

(loop [i 0
       robots input-data]
  (do
    (when (< start-printing i)
      (do
        (print-robots (map first robots))
        (print i)
        (flush)
        (read-line)))
    (recur (inc i) (map advance-position robots))))
