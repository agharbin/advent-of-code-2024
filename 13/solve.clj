(ns advent.2024.13
  (:require
    [clojure.string :as s]))

(def a-cost 3)
(def b-cost 1)

;; IO

(defn parse-section [input]
  (let [[a-line b-line prize-line] (s/split-lines input)
        [_ ax-str ay-str] (re-matches #"Button A: X\+(\d+), Y\+(\d+)" a-line)
        [_ bx-str by-str] (re-matches #"Button B: X\+(\d+), Y\+(\d+)" b-line)
        [_ px-str py-str] (re-matches #"Prize: X=(\d+), Y=(\d+)" prize-line)]
    {:ax (parse-long ax-str)
     :ay (parse-long ay-str)
     :bx (parse-long bx-str)
     :by (parse-long by-str)
     :px (parse-long px-str)
     :py (parse-long py-str)}))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (map parse-section)))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(defn find-solutions [coefficients]
   (let [K (:ax coefficients)
         L (:bx coefficients)
         M (:ay coefficients)
         N (:by coefficients)
         X (:px coefficients)
         Y (:py coefficients)
         b (/ (- (* Y K) (* M X))
              (- (* N K) (* M L)))
         a (/ (- X (* L b))
              K)]
   [a b]))

(defn score [coefficients]
  (let [[a b] (find-solutions coefficients)]
    (if (and (int? a) (int? b))
      (+ (* a-cost a) (* b-cost b))
      0)))

(defn solve [input]
  (->> input
       (map score)
       (apply +)))

(solve input-data)

;; Part 2

(def constant-to-add 10000000000000)
(defn add-constant [x] (+ x constant-to-add))

(defn solve-2 [input]
  (->> input
       (map (fn [m] (-> m
                        (update :px add-constant)
                        (update :py add-constant))))
       (map score)
       (apply +)))

(solve-2 input-data)
