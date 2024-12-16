(ns aoc2
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (map #(Integer/parseInt %) (clojure.string/split line #" +")))

(defn parse-file [file]
  (let [lines (string/split-lines (slurp file))]
    (map parse-line lines)))

(defn is-safe? [l]
  (if (or (empty? l) (= 1 (count l)))
    false
    (let [l2 (rest l)
          diffs (map - l2 l)
          adiffs (map abs diffs)
          signs (map #(if (pos? %) 1 -1) diffs)
          flat? (some zero? diffs)
          descending? (not (some pos? signs))
          ascending? (not (some neg? signs))
          incremental? (not (some #(> % 3) adiffs))
          safe? (and
                  (not flat?)
                  (or ascending?
                      descending?)
                  incremental?)]
      safe?)))

(defn dampen-report [report]
  (let [n (count report)]
    (for [i (range n)]
      (vec (concat (take i report) (drop (inc i) report))))))

(defn is-dampened-safe? [report]
  (boolean (some is-safe? (dampen-report report))))