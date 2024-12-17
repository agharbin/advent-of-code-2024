(ns advent.2024
  (:require
    [clojure.data.priority-map :as pm]
    [clojure.string :as s]
    [clojure.set :as set]
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
(def rows (count input-grid))
(def cols (count (first input-grid)))
(def all-positions (for [r (range rows) c (range cols)] [r c]))
(def start-position (first (filter #(= \S (get-in input-grid %)) all-positions)))
(def goal-position (first (filter #(= \E (get-in input-grid %)) all-positions)))

;; Part 1

(def all-facings [[0 1] [1 0] [0 -1] [-1 0]])
(def start-facing [0 1])

(def rotate-right
  {[0 1] [1 0]
   [1 0] [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]})

(def rotate-left (set/map-invert rotate-right))

(defn goal? [position]
  (= goal-position position))

(defn forward-clear? [[position facing]]
  (#{\. \E} (get-in input-grid (u/vector+ position facing))))

(defn valid-next-nodes [[[position facing] score]]
  (cond-> []
    (forward-clear? [position facing])
      (conj [[(u/vector+ position facing) facing] (inc score)])
    (forward-clear? [position (rotate-left facing)])
      (conj [[position (rotate-left facing)] (+ score 1000)])
    (forward-clear? [position (rotate-right facing)])
      (conj [[position (rotate-right facing)] (+ score 1000)])))

(def get-position first)

(defn run-dijkstra [start-node]
  (loop [q (conj (pm/priority-map) [start-node 0])
         node->length (->> (for [p all-positions f all-facings] [p f])
                           (filter #(#{\E \. \S} (get-in input-grid (get-position %))))
                           (map (fn [x] [x Integer/MAX_VALUE]))
                           (into {}))]
    (if (seq q)
      (let [node (peek q)
            candidates (valid-next-nodes node)
            better-candidates (filter (fn [[new-node new-score]] (< new-score (node->length new-node)))
                                      candidates)]
        (recur
          (into (pop q) better-candidates)
          (into node->length better-candidates)))
      (apply min (for [f all-facings] (node->length [goal-position f]))))))

(defn solve [input]
  (run-dijkstra [start-position start-facing]))

(solve input-grid)

;; Part 2

(defn trace-backward [node->length backward-edges]
  (let [backward-edges (dissoc backward-edges [start-position start-facing])
        goal-node (first (sort-by #(node->length %) (for [f all-facings] [goal-position f])))]
    (loop [xs [goal-node]
           found-positions #{(get-position goal-node)}]
      (if (seq xs)
        (let [item (first xs)
              position (get-position item)]
          (recur (into (rest xs) (backward-edges item)) (conj found-positions position)))
        (count found-positions)))))

(defn run-dijkstra-2 [start-node]
  (loop [q (conj (pm/priority-map) [start-node 0])
         node->length (->> (for [p all-positions f all-facings] [p f])
                        (filter #(#{\E \. \S} (get-in input-grid (get-position %))))
                        (map (fn [x] [x Integer/MAX_VALUE]))
                        (into {}))
         backward-edges {}]
    (if (seq q)
      (let [node (peek q)
            candidates (valid-next-nodes node)
            better-candidates (filter (fn [[new-node new-score]] (< new-score (node->length new-node)))
                                      candidates)
            equal-candidates (filter (fn [[new-node new-score]] (= new-score (node->length new-node)))
                                     candidates)
            new-backward-edges (into backward-edges
                                     (for [c better-candidates] [(get-position c) #{(get-position node)}]))
            new-backward-edges
              (reduce
                (fn [m c] (update m (first c) #(conj % (first node))))
                new-backward-edges
                equal-candidates)]
        (recur
          (into (pop q) better-candidates)
          (into node->length better-candidates)
          new-backward-edges))
      (trace-backward node->length backward-edges))))

(defn solve-2 [input]
  (run-dijkstra-2 [start-position start-facing]))

(solve-2 input-grid)
