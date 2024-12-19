(ns advent.2024.18
  (:require
    [clojure.data.priority-map :as pm]
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-line [input]
  (mapv parse-long (re-seq #"\d+" input)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(def space-size 71)
(def num-bytes 1024)
(def start-space [0 0])
(def goal-space [(dec space-size) (dec space-size)])

(defn valid-next-nodes-and-distances [bytes-set [space distance]]
  (->> (for [offset [[1 0] [-1 0] [0 1] [0 -1]]] (u/vector+ space offset))
       (filter #(and (<= 0 (first %) (dec space-size)) (<= 0 (second %) (dec space-size))))
       (filter #(not (bytes-set %)))
       (map #(vector % (inc distance)))))

(defn run-dijkstra [bytes-set start-space]
  (loop [q (conj (pm/priority-map) [start-space 0])
         space->distance (->> (for [r (range space-size) c (range space-size)] [r c])
                           (map (fn [x] [x Integer/MAX_VALUE]))
                           (into {}))]
    (if (seq q)
      (let [space-and-distance (peek q)
            candidates (valid-next-nodes-and-distances bytes-set space-and-distance)
            better-candidates (filter (fn [[next-space next-distance]] (< next-distance (space->distance next-space)))
                                      candidates)]
        (recur
          (into (pop q) better-candidates)
          (into space->distance better-candidates)))
      (space->distance goal-space))))

(defn solve [bytes-vec]
  (run-dijkstra (into #{} bytes-vec) start-space))

(solve (take num-bytes input-data))

;; Part 2

(defn solve-2 [bytes-vec]
  (loop [xs bytes-vec
         fallen-bytes #{}]
    (if (= Integer/MAX_VALUE (run-dijkstra (conj fallen-bytes (first xs)) start-space))
      (first xs)
      (recur (rest xs) (conj fallen-bytes (first xs))))))

(solve-2 input-data)
