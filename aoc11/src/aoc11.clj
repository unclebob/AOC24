(ns aoc11
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %)
        (string/split
          (string/trim line) #" ")))

(defn parse-file [filename]
  (-> filename slurp parse-line))

(defn split-stone [stone]
  (let [s (str stone)]
    [(Integer/parseInt (subs s 0 (/ (count s) 2)))
     (Integer/parseInt (subs s (/ (count s) 2)))]))

(defn blink-one [stone]
  (cond
    (zero? stone) 1
    (even? (count (str stone))) (split-stone stone)
    :else (* 2024 stone)))

(defn blink-stones-m [stones]
  (loop [stones stones
         new-stones []]
    (if (empty? stones)
      new-stones
      (let [new-stone (blink-one (first stones))]
        (if (number? new-stone)
          (recur (rest stones)
                 (conj new-stones new-stone))
          (recur (rest stones)
                 (conj new-stones
                       (first new-stone)
                       (second new-stone))))))))

(def blink-stones (memoize blink-stones-m))

(defn blink-stones-n-times [n stones]
  (loop [n n
         stones stones]
    (prn 'n n 'stones (count stones))
    (if (zero? n)
      stones
      (recur (dec n)
             (blink-stones stones)))))

(defn solve-1 [filename]
  (let [stones (parse-file filename)]
    (count (blink-stones-n-times 25 stones))))

(defn solve-2 [filename]
  (let [stones (parse-file filename)]
    (count (blink-stones-n-times 75 stones))))