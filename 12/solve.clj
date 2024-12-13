(ns advent.2024.12
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

(def input-filename "input.dat")
(def input-grid (->> (slurp input-filename) parse-input))

;; Part 1

(defn find-neighbors [grid position]
  (->> (for [offset [[1 0] [-1 0] [0 1] [0 -1]]] (u/vector+ position offset))
       (into #{})))

(defn flood-fill [grid start-position]
  (let [plant-type (get-in grid start-position)]
    (loop [positions-to-search #{start-position}
           reached #{}
           perimeter 0
           area 0]
      (if (seq positions-to-search)
        (let [next-position (first positions-to-search)
              neighbors (find-neighbors grid next-position)
              unexplored-neighbors (->> neighbors
                                        (filter #(get-in grid %))
                                        (filter #(= plant-type (get-in grid %)))
                                        (filter #(not (reached %)))
                                        (filter #(not (positions-to-search %))))
              sides-in-perimeter (->> neighbors
                                      (map #(get-in grid % \0))
                                      (filter #(not= plant-type %))
                                      count)]
          (recur (into (disj positions-to-search next-position) unexplored-neighbors)
                 (conj reached next-position)
                 (+ perimeter sides-in-perimeter)
                 (inc area)))
        [reached (* area perimeter)]))))

(defn solve [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [positions-to-search (for [r (range rows) c (range cols)] [r c])
           reached #{}
           total-score 0]
      (if (seq positions-to-search)
        (let [next-position (first positions-to-search)]
          (if (reached next-position)
            (recur (rest positions-to-search) reached total-score)
            (let [[newly-reached score] (flood-fill grid next-position)]
              (recur (rest positions-to-search) (into reached newly-reached) (+ total-score score)))))
        total-score))))

(solve input-grid)

;; Part 2

(defn flood-fill-2 [grid start-position]
  (let [plant-type (get-in grid start-position)]
    (loop [positions-to-search #{start-position}
           reached #{}
           area 0]
      (if (seq positions-to-search)
        (let [next-position (first positions-to-search)
              neighbors (find-neighbors grid next-position)
              unexplored-neighbors (->> neighbors
                                        (filter #(get-in grid %))
                                        (filter #(= plant-type (get-in grid %)))
                                        (filter #(not (reached %)))
                                        (filter #(not (positions-to-search %))))]
          (recur (into (disj positions-to-search next-position) unexplored-neighbors)
                 (conj reached next-position)
                 (inc area)))
        [(into {} (for [s reached] [s start-position])) {start-position area}]))))

(defn count-corners [grid region-map]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [positions (for [r (range -1 rows) c (range -1 cols)] [r c])
           region->side-count {}]
      (if (seq positions)
        ; We examine 4 squares forming a box. If a region is represented 1 or 3 times, this is
        ; the corner of a rectangular area of that region. If 2 squares in the box belong to the
        ; same region, both are corners only if they are diagonal to each other.
        (let [position (first positions)
              squares [position
                       (u/vector+ position [1 0])
                       (u/vector+ position [0 1])
                       (u/vector+ position [1 1])]
              region-frequencies (->> squares (map region-map) frequencies)
              corner-squares (->> region-frequencies
                                  (filter (fn [[k v]] (or (= 1 v) (= 3 v))))
                                  (map first))
              double-corners (->> region-frequencies
                                  (filter (fn [[k v]]
                                            (and (= 2 v)
                                                 (or (= k
                                                        (region-map position)
                                                        (region-map (u/vector+ position [1 1])))
                                                     (= k
                                                        (region-map (u/vector+ position [1 0]))
                                                        (region-map (u/vector+ position [0 1])))))))
                                  (map first))]
          (recur
            (rest positions)
            (merge-with +
                        region->side-count
                        (into {} (for [r corner-squares] [r 1]))
                        (into {} (for [r double-corners] [r 2])))))
        region->side-count))))

(defn solve-2 [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (loop [positions-to-search (for [r (range rows) c (range cols)] [r c])
           region-map {}
           region-areas {}]
      (if (seq positions-to-search)
        (let [next-position (first positions-to-search)]
          (if (region-map next-position)
            (recur (rest positions-to-search) region-map region-areas)
            (let [[region area] (flood-fill-2 grid next-position)]
              (recur (rest positions-to-search) (merge region-map region) (merge region-areas area)))))
        (let [side-count (count-corners grid region-map)
              regions (into #{} (vals region-map))]
          (apply +
            (for [r regions] (* (region-areas r) (side-count r)))))))))

(solve-2 input-grid)
