(ns advent.2024.11)

;; IO

(defn parse-input [input]
   (map parse-long (re-seq #"\d+" input)))

(def input-file "input.dat")
(def input-stones (->> (slurp input-file) parse-input frequencies))

;; Part 1

(defn next-stones [stone]
  (let [stone-str (str stone)
        length (count stone-str)
        half-length (/ length 2)]
    (cond
      (zero? stone)
        [1]
      (even? length)
        [(->> stone-str (take half-length) (apply str) parse-long)
         (->> stone-str (drop half-length) (apply str) parse-long)]
      :else
        [(* stone 2024)])))

(defn next-state [state]
  (reduce
    (fn [m [k v]]
      (apply merge-with + m (for [s (next-stones k)] {s v})))
    {}
    state))

(defn solve [input iterations]
  (loop [m input
         i 0]
    (if (= i iterations)
      (apply + (vals m))
      (recur (next-state m) (inc i)))))

(solve input-stones 25)

;; Part 2

(solve input-stones 75)
