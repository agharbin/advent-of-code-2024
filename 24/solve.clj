(ns advent.2024.24
  (:require
    [clojure.string :as s]
    [util :as u]))

;; IO

(defn parse-gate [input]
  (let [[_ input1 op input2 output] (re-matches #"(\w+) (\w+) (\w+) -> (\w+)" input)]
    [output {:inputs [input1 input2] :op op}]))

(defn parse-gates [input]
  (->> input
       (s/split-lines)
       (mapv parse-gate)
       (into {})))

(defn parse-initial-value [input]
  (let [[_ node val-str] (re-matches #"(\w+): (\d+)" input)]
    [node (parse-long val-str)]))

(defn parse-initial-values [input]
  (->> input
       (s/split-lines)
       (map parse-initial-value)
       (into {})))

(defn parse-input [input]
  (let [[initial-values-str gates-str] (s/split input #"\n\n")]
    {:init-values (parse-initial-values initial-values-str)
     :gates (parse-gates gates-str)}))

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))

;; Part 1

(def compute-value
  (memoize
    (fn [node gates init-values]
      (if (init-values node)
        (init-values node)
        (let [[input-1 input-2] (:inputs (gates node))
              op (:op (gates node))
              val-1 (compute-value input-1 gates init-values)
              val-2 (compute-value input-2 gates init-values)]
          (case op
            "AND" (bit-and val-1 val-2)
            "OR" (bit-or val-1 val-2)
            "XOR" (bit-xor val-1 val-2)))))))

(defn compute-binary-value [xs]
  (->> (u/zip xs (iterate #(* 2 %) 1))
       (map #(apply * %))
       (apply +)))

(defn solve [{:keys [gates init-values]}]
  (let [all-nodes (keys gates)
        z-nodes (->> all-nodes (filter #(= \z (first %))) sort)]
    (compute-binary-value
      (map #(compute-value % gates init-values) z-nodes))))

(solve input-data)

;; Part 2

(defn apply-corrections [gates]
  (let [z10 (gates "z10")
        kmb (gates "kmb")
        z15 (gates "z15")
        tvp (gates "tvp")
        z25 (gates "z25")
        dpg (gates "dpg")
        vdk (gates "vdk")
        mmf (gates "mmf")]
    (-> gates
        (assoc "kmb" z10)
        (assoc "z10" kmb)
        (assoc "z15" tvp)
        (assoc "tvp" z15)
        (assoc "z25" dpg)
        (assoc "dpg" z25)
        (assoc "vdk" mmf)
        (assoc "mmf" vdk))))

(defn print-dot-file [{:keys [gates init-values]}]
  (let [gates (apply-corrections gates)
        all-nodes (into (keys gates) (mapcat :inputs (vals gates)))
        z-nodes (->> all-nodes (filter #(= \z (first %))) sort)
        x-nodes (->> all-nodes (filter #(= \x (first %))) sort)
        y-nodes (->> all-nodes (filter #(= \y (first %))) sort)
        z-text (str "{ rank=same; " (s/join \, z-nodes) " } \n")
        x-text (str "{ rank=same; " (s/join \, x-nodes) " } \n")
        y-text (str "{ rank=same; " (s/join \, y-nodes) " } \n")
        preamble "digraph aoc24 {\n"
        edge-text (loop [xs (sort-by first gates)
                         i 0
                         result []]
                    (if (seq xs)
                      (let [[output {:keys [inputs op]}] (first xs)]
                        (recur (rest xs) (inc i) (into result
                                                       [(str (first inputs) " -> " op i ";")
                                                        (str (second inputs) " -> " op i ";")
                                                        (str op i " -> " output ";")])))
                      (s/join \newline result)))
        end "}\n"]
   (spit "graph.dot" (str preamble edge-text \newline z-text x-text y-text end))))

(defn print-z-wires []
  (let [gates (apply-corrections (:gates input-data))
        all-nodes (into (keys gates) (mapcat :inputs (vals gates)))
        z-nodes (->> all-nodes (filter #(= \z (first %))) sort)]
    (loop [xs z-nodes]
      (when (seq xs)
        (let [n (first xs)
              inputs (:inputs (gates n))
              op (:op (gates n))
              left (if (gates (first inputs))
                     (gates (first inputs))
                     ((:init-values input-data) (first inputs)))
              right (if (gates (second inputs))
                     (gates (second inputs))
                     ((:init-values input-data) (second inputs)))]
          (prn "Node:" n "   " op)
          (prn left)
          (prn right)
          (prn)
          (recur (rest xs)))))))
