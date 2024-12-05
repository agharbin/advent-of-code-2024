(ns advent.2024.4
  (:require
    [clojure.string :as s]
    [util :as u]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map vec)
       vec))

(defn find-xmas [input pos]
  (apply +
    (for [offset [[1 0] [0 1] [-1 0] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]]
      (let [a (get-in input (u/vector+ pos offset) \space)
            b (get-in input (u/vector+ pos (map #(* 2 %) offset)) \space)
            c (get-in input (u/vector+ pos (map #(* 3 %) offset)) \space)]
        (if (= [\X \M \A \S] [\X a b c])
          1
          0)))))

(defn find-x-mas [input pos]
  (let [ul (get-in input (u/vector+ pos [-1 -1]) \space)
        ur (get-in input (u/vector+ pos [1 -1]) \space)
        ll (get-in input (u/vector+ pos [-1 1]) \space)
        lr (get-in input (u/vector+ pos [1 1]) \space)]
    (if (and
          (or (= [ul lr] [\M \S]) (= [ul lr] [\S \M]))
          (or (= [ur ll] [\M \S]) (= [ur ll] [\S \M])))
      1
      0)))

(defn solve [start-char find-fn input]
  (let [rows (count input)
        cols (count (first input))]
    (apply +
      (for [r (range rows) c (range cols)]
        (if (= start-char (get-in input [r c]))
          (find-fn input [r c])
          0)))))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       (solve \A find-x-mas)
       prn))

(solve-file "input.dat")
