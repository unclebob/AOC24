(ns aoc1
  (:require [clojure.string :as string]))

(defn diff-of-sorted-elements [l1 l2]
  (let [l1 (sort l1)
        l2 (sort l2)
        ld (map #(abs (- %1 %2)) l1 l2)
        d (reduce + ld)]
    d))

(defn parse-line [line]
  (map #(Integer/parseInt %) (string/split line #" +")))

(defn parse-lines [lines]
  (let [pairs (map parse-line lines)
        l1 (map first pairs)
        l2 (map second pairs)]
    [l1 l2]))

(defn parse-file [filename]
  (let [lines (slurp filename)]
    (parse-lines (string/split-lines lines))))

(defn solve-1 [filename]
  (let [[l1 l2] (parse-file filename)]
    (diff-of-sorted-elements l1 l2)))

(defn create-frequency-list [[l1 l2]]
  (for [n l1] [n (count (filter #(= % n) l2))]))

(defn solve-2 [filename]
  (let [input (parse-file filename)
        freqs (create-frequency-list input)
        products (map (fn [[n f]] (* n f)) freqs)]
    (reduce + products)))