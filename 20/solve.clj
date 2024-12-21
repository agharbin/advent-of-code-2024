(ns advent.2024.20
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-line [input]
  (vec input))

(defn parse-input [input]
  (->> input
       s/split-lines
       (mapv parse-line)))

(def input-file "input.dat")
(def grid (->> (slurp input-file) parse-input))
(def rows (count grid))
(def cols (count (first grid)))

;; Part 1

(defn has-contents? [symbol-set]
  (fn [position] (symbol-set (get-in grid position))))

(def all-positions (for [r (range rows) c (range cols)] [r c]))
(def start-position (->> all-positions
                         (filter (has-contents? #{\S}))
                         first))
(def goal-position (->> all-positions
                        (filter (has-contents? #{\E}))
                        first))
(def path-positions (->> all-positions
                         (filter (has-contents? #{\E \S \.}))))

(defn find-possible-next-positions [position]
  (->> (u/four-neighbors position)
       (filter (has-contents? #{\E \S \.}))))

(defn find-distances-from-position [p]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [p 0])
         position->distance {}]
    (if (seq q)
      (let [[position distance] (peek q)]
        (if (position->distance position)
          (recur (pop q) position->distance)
          (let [candidates (find-possible-next-positions position)]
            (recur
              (into (pop q) (for [s candidates] [s (inc distance)]))
              (conj position->distance [position distance])))))
      position->distance)))

(def position->distance-from-goal (find-distances-from-position goal-position))
(def position->distance-from-start (find-distances-from-position start-position))
(def shortest-path-length (position->distance-from-start goal-position))

(defn compute-jump-distance [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn find-distances-with-cheat [cheat-length]
  (->> (for [cheat-start path-positions cheat-end path-positions] [cheat-start cheat-end])
       (filter (fn [[cheat-start cheat-end]] (<= (compute-jump-distance cheat-start cheat-end) cheat-length)))
       (map (fn [[cheat-start cheat-end]] (- shortest-path-length
                                             (+ (compute-jump-distance cheat-start cheat-end)
                                                (position->distance-from-start cheat-start)
                                                (position->distance-from-goal cheat-end)))))
       (filter #(<= 100 %))))

(count (find-distances-with-cheat 2))

;; Part 2

(count (find-distances-with-cheat 20))
