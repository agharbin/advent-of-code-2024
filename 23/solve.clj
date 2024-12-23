(ns advent.2024.23
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

;; IO

(defn parse-line [input]
  (vec (re-seq #"\w+" input)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(defn build-adjacency-list [input]
  (loop [xs input
         result {}]
    (if (seq xs)
      (let [[v1 v2] (first xs)]
        (recur
          (rest xs)
          (cond-> result
            (not (result v1)) (assoc v1 #{})
            (not (result v2)) (assoc v2 #{})
            true (update v1 #(conj % v2))
            true (update v2 #(conj % v1)))))
      result)))

(defn find-triangles [adjacency-list]
  (let [all-nodes (keys adjacency-list)
        all-pairs (for [n1 all-nodes n2 all-nodes :when (not= n1 n2)] [n1 n2])]
    (loop [xs all-pairs
           triangles #{}]
      (if (seq xs)
        (let [[n1 n2] (first xs)]
          (if ((adjacency-list n1) n2)
            (let [common-neighbors (set/intersection (adjacency-list n1) (adjacency-list n2))]
              (recur (rest xs) (into triangles (for [c common-neighbors] #{n1 n2 c}))))
            (recur (rest xs) triangles)))
        triangles))))

(defn solve [input]
  (let [adjacency-list (build-adjacency-list input)
        triangles (find-triangles adjacency-list)
        t-nodes (->> (keys adjacency-list) (filter #(= \t (first %))) (into #{}))
        triangles-containing-t-nodes (->> triangles (filter #(seq (set/intersection % t-nodes))))]
    (count triangles-containing-t-nodes)))

(solve input-data)

;; Part 2

(defn expand-clique [initial-set adjacency-list]
  (loop [xs (keys adjacency-list)
         s initial-set]
    (if (seq xs)
      (let [c (first xs)]
        (cond
          (s c) (recur (rest xs) s)
          (= s (set/intersection (adjacency-list c) s)) (recur (rest xs) (conj s c))
          :else (recur (rest xs) s)))
      s)))

(defn solve-2 [input]
  (let [adjacency-list (build-adjacency-list input)
        triangles (find-triangles adjacency-list)]
    (loop [xs triangles
           max-found (first triangles)]
      (if (seq xs)
        (let [t (first xs)
              clique (expand-clique t adjacency-list)]
          (if (< (count max-found) (count clique))
            (recur (rest xs) clique)
            (recur (rest xs) max-found)))
        (s/join \, (sort max-found))))))

(solve-2 input-data)
