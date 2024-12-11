(ns advent.2024.9
  (:require
    [clojure.string :as s]))

;; IO

(defn parse-input [input]
  (loop [xs (s/trim input)
         next-id 0
         next-entry :file
         result []]
    (if (seq xs)
      (let [s (parse-long (str (first xs)))]
        (cond
          (= :file next-entry)
            (recur (rest xs) (inc next-id) :free (conj result {:type :file :id next-id :size s}))
          (and (= :free next-entry) (zero? s))
            ; Don't need to track length-0 free spaces
            (recur (rest xs) next-id :file result)
          :else
            (recur (rest xs) next-id :file (conj result {:type :free :size s}))))
      result)))

(def input-file "input.dat")
(def input (parse-input (slurp input-file)))

;; Part 1

(defn compact [filemap]
  (let [filemap-size (apply + (map :size filemap))]
    (loop [forward-list (seq filemap)
           forward-index 0
           backward-list (reverse (seq filemap))
           backward-index (dec filemap-size)
           result []]
      (let [front-entry (first forward-list)
            back-entry (first backward-list)]
        (cond
          (= backward-index forward-index)
            ; If the indexes meet, we partially copied a file in the middle of the map
            (conj result (first backward-list))
          (< backward-index forward-index)
            ; If the indexes cross, we've finished compaction
            result
          (= :file (:type front-entry))
            ;; Skip file to find next free space at front of list
            (recur
              (rest forward-list)
              (+ forward-index (:size front-entry))
              backward-list
              backward-index
              (conj result front-entry))
          (= :free (:type back-entry))
            ;; Skip over free spaces at back of list to find next file to copy
            (recur forward-list forward-index (rest backward-list) (- backward-index (:size back-entry)) result)
          :else
            ;; Copy the file at `back-entry` to the free space at `front-entry`, creating new partial entries
            (let [free front-entry
                  file back-entry
                  blocks-to-copy (min (:size free) (:size file))
                  remaining-in-free (- (:size free) blocks-to-copy)
                  remaining-in-file (- (:size file) blocks-to-copy)]
              (recur
                (if (< 0 remaining-in-free)
                  (conj (rest forward-list) {:type :free :size remaining-in-free})
                  (rest forward-list))
                (+ forward-index blocks-to-copy)
                (if (< 0 remaining-in-file)
                  (conj (rest backward-list) (assoc file :size remaining-in-file))
                  (rest backward-list))
                (- backward-index blocks-to-copy)
                (conj result (assoc file :size blocks-to-copy)))))))))

(defn calculate-score [filemap]
  (loop [xs filemap
         index 0
         score 0]
    (if (seq xs)
      (let [f (first xs)]
        (if (= :free (:type f))
          (recur (rest xs) (+ index (:size f)) score)
          (recur
            (rest xs)
            (+ index (:size f))
            (+ score (->> (range index (+ index (:size f)))
                          (map #(* (:id f) %))
                          (apply +))))))
      score)))

(defn solve [filemap]
  (calculate-score (compact filemap)))

(solve input)

;; Part 2

(defn move-whole-file [filemap file]
  (loop [xs filemap
         result []]
    (when (seq xs)
      (let [space (first xs)]
        (cond
          (= (:id file) (:id space))
            ; We only consider positions to the left, so if we hit this file, we're done
            filemap
          (or (= :file (:type space)) (< (:size space) (:size file)))
            ; Not a free space; or too small
            (recur (rest xs) (conj result space))
          :else
            ; This space is free and big enough to take the file
            (let [remaining-in-free (- (:size space) (:size file))
                  rest-of-map-except-file
                    (map #(if (= (:id %) (:id file)) (-> % (assoc :type :free) (dissoc :id)) %)
                         (rest xs))]
                (if (zero? remaining-in-free)
                  (into (conj result file) rest-of-map-except-file)
                  (into (-> result (conj file) (conj {:type :free :size remaining-in-free}))
                        rest-of-map-except-file))))))))

(defn compact-2 [filemap]
  (let [filemap-size (apply + (map :size filemap))]
    (loop [backward-list (reverse (seq filemap))
           result filemap]
      (if (seq backward-list)
        (let [back-entry (first backward-list)]
          (if (= :free (:type back-entry))
            (recur (rest backward-list) result)
            (recur (rest backward-list) (move-whole-file result (first backward-list)))))
        result))))

(defn solve-2 [filemap]
  (calculate-score (compact-2 filemap)))

(solve-2 input)
