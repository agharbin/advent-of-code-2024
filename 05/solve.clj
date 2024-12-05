(ns advent.2024.5
  (:require
    [clojure.string :as s]
    [clojure.set :as set]))

(defn parse-input [input]
  (let [[rules-section updates-section] (s/split input #"\n\n")
        rules-strings (s/split-lines rules-section)
        updates-strings (s/split-lines updates-section)
        rules (mapv (fn [x] (mapv parse-long (s/split x #"\|"))) rules-strings)
        updates (mapv (fn [x] (mapv parse-long (s/split x #","))) updates-strings)]
    [rules updates]))

(defn topological-sort [rules]
  (let [all-nodes (into #{} (apply concat rules))
        init-incoming-edge-counts (frequencies (map second rules))
        no-incoming-edges (set/difference all-nodes (into #{} (keys init-incoming-edge-counts)))
        init-incoming-edge-counts (reduce (fn [m k] (assoc m k 0)) init-incoming-edge-counts no-incoming-edges)]
    (loop [result []
           incoming-edge-counts init-incoming-edge-counts]
      (if (seq incoming-edge-counts)
        (let [start-node (->> incoming-edge-counts (filter #(= 0 (second %))) first first)
              edges-from-start-node (filter #(= (first %) start-node) rules)
              nodes-to-decrement-incoming-edge-count (map second edges-from-start-node)]
          (recur
            (conj result start-node)
            (reduce
              (fn [m k] (update m k dec))
              (dissoc incoming-edge-counts start-node)
              nodes-to-decrement-incoming-edge-count)))
        result))))

(defn get-index->node [rules nodes]
  (let [nodes-set (into #{} nodes)
        relevant-rules (filter #(and (nodes-set (first %)) (nodes-set (second %))) rules) ]
    (topological-sort relevant-rules)))

(defn is-update-valid? [rules update-vec]
  (let [index->node (get-index->node rules update-vec)
        node->index (set/map-invert index->node)
        indexes (map node->index update-vec)]
    (apply < indexes)))

(defn sort-updates [rules update-vec]
  (let [index->node (get-index->node rules update-vec)
        node->index (set/map-invert index->node)]
    (->> update-vec
         (mapv node->index)
         sort
         (mapv index->node))))

(defn get-middle-value [xs]
  (xs (int (/ (count xs) 2))))

(defn solve [[rules updates]]
  (->> updates
       (filter #(is-update-valid? rules %))
       (map get-middle-value)
       (apply +)))

(defn solve-2 [[rules updates]]
  (->> updates
       (filter #(not (is-update-valid? rules %)))
       (mapv #(sort-updates rules %))
       (mapv get-middle-value)
       (apply +)))

(defn solve-file [input]
  (->> (slurp input)
       parse-input
       solve-2
       prn))

(solve-file "input.dat")
