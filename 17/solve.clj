(ns advent.2024.17
  (:require
    [clojure.string :as s]))

;; IO

(defn parse-input [input]
  (let [[a-str b-str c-str _ program-str] (s/split-lines input)
        a (-> (re-matches #"Register A: (\d+)" a-str) second parse-long)
        b (-> (re-matches #"Register B: (\d+)" b-str) second parse-long)
        c (-> (re-matches #"Register C: (\d+)" c-str) second parse-long)
        program (mapv parse-long (re-seq #"\d+" program-str))]
    [a b c program]))

(defn to-state [[a b c program]]
  {:a a
   :b b
   :c c
   :pc 0
   :program program
   :output []})

(def input-file "input.dat")
(def input-data (->> (slurp input-file) parse-input))
(def initial-state (to-state input-data))
(def program (:program initial-state))
(def program-length (count program))

;; Part 1

(defn combo-operand [operand state]
  (cond
    (<= 0 operand 3) operand
    (= 4 operand) (:a state)
    (= 5 operand) (:b state)
    (= 6 operand) (:c state)))

(defn plus-two [x]
  (+ x 2))

(defn next-state [{:keys [a b c pc program] :as state}]
  (let [opcode (program pc)
        operand (program (inc pc))]
    (case opcode
      0 (-> state
            (assoc :a (long (/ a (Math/pow 2 (combo-operand operand state)))))
            (update :pc plus-two))
      1 (-> state
            (assoc :b (bit-xor b operand))
            (update :pc plus-two))
      2 (-> state
            (assoc :b (mod (combo-operand operand state) 8))
            (update :pc plus-two))
      3 (if (zero? a)
          (update state :pc plus-two)
          (assoc state :pc operand))
      4 (-> state
            (assoc :b (bit-xor b c))
            (update :pc plus-two))
      5 (-> state
            (update :output #(conj % (mod (combo-operand operand state) 8)))
            (update :pc plus-two))
      6 (-> state
            (assoc :b (long (/ a (Math/pow 2 (combo-operand operand state)))))
            (update :pc plus-two))
      7 (-> state
            (assoc :c (long (/ a (Math/pow 2 (combo-operand operand state)))))
            (update :pc plus-two)))))

(defn solve []
  (loop [s initial-state]
    (if (< (:pc s) program-length)
      (recur (next-state s))
      (s/join \, (:output s)))))

(solve)

;; Part 2

(defn run-program [start-state]
  (loop [s start-state]
    (if (< (:pc s) program-length)
      (recur (next-state s))
      (:output s))))

(defn from-octal [digits]
  (loop [a 0
         i 1
         xs (reverse digits)]
    (if (seq xs)
      (recur (+ a (* i (first xs))) (* 8 i) (rest xs))
      a)))

(defn find-a [guess]
  (loop [xs (range 0 8)]
    (if (seq xs)
      (let [new-input (conj guess (first xs))
            input-value (from-octal new-input)
            result (run-program (assoc initial-state :a input-value))
            result-length (count result)
            program-suffix (subvec program (- program-length result-length))]
        (cond
          (= result program) input-value
          (= result program-suffix) (let [r (find-a new-input)]
                                      (if (some? r)
                                        r
                                        (recur (rest xs))))
          :else (recur (rest xs))))
      nil)))

(defn solve-2 []
  (find-a []))

(solve-2)
