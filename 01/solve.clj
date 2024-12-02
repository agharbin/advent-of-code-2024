(ns advent.2024.1
  (:require
    [clojure.string :as s]
    [util :as u]))

(defn parse-line [input]
  (let [[_ n1 n2] (re-matches #"(\d+)\s+(\d+)" input)]
    [(parse-long n1) (parse-long n2)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       u/transpose))

(defn solve [[list1 list2]]
 (let [sorted1 (sort list1)
       sorted2 (sort list2)
       pairs (u/transpose [sorted1 sorted2])]
   (->> pairs
     (map (fn [[x y]] (abs (- x y))))
     (apply +))))

(defn solve-2 [[list1 list2]]
  (let [freqs (frequencies list2)]
    (->> list1
         (map #(* % (get freqs % 0)))
         (apply +))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve-2
       prn))

(solve-file "input.dat")
