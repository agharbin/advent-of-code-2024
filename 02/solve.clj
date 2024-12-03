(ns advent.2022.2
  (:require
    [clojure.string :as s]))

(defn parse-line [input]
  (->> input
    (re-seq #"\d+")
    (map parse-long)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (map vec)))

(defn max-difference [report]
  (every? true? (map (fn [[x y]] (< (abs (- x y)) 4)) (partition 2 1 report))))

(defn check-report [report]
    (if
      (and
        (or
          (apply < report)
          (apply > report))
        (max-difference report))
      1
      0))

(defn delete [report i]
  (concat (subvec report 0 i)
          (subvec report (inc i))))

(defn check-report-2 [report]
  (if
    (some pos?
      (for [i (range (count report))]
        (check-report (delete report i))))
    1
    0))

(defn solve [input]
  (->> input
       (map check-report-2)
       (apply +)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve
       prn))

(solve-file "input.dat")
