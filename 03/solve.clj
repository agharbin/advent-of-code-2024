(ns advent.2024.3)

(defn solve [input]
  (->> input
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (fn [[_ x y]] [(parse-long x) (parse-long y)]))
       (map #(apply * %))
       (apply +)))

(defn solve-2 [input]
  (loop [xs (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" input)
         on? true
         acc 0]
    (if (seq xs)
      (let [[op x y] (first xs)]
        (cond (= op "do()") (recur (rest xs) true acc)
              (= op "don't()") (recur (rest xs) false acc)
              on? (recur (rest xs) on? (+ acc (* (parse-long x) (parse-long y))))
              :else (recur (rest xs) on? acc)))
      acc)))

(defn solve-file [input]
  (->> (slurp input)
       solve-2
       prn))

(solve-file "input.dat")
