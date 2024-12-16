(ns aoc7
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(def ops (atom []))

(defn parse-line [line]
  (mapv #(Long/parseLong %) (re-seq #"\d+" line)))

(defn parse-lines [lines]
  (mapv parse-line lines))

(defn parse-file [filename]
  (-> filename slurp (string/split-lines) parse-lines))

(defn cat-nums [a b]
  (Long/parseLong (str a b)))

(defn eval-formula [ns ops]
  (loop [nums (rest ns)
         ops ops
         acc (first ns)]
    (if (empty? nums)
      acc
      (let [op (first ops)
            num (first nums)]
        (recur (rest nums) (rest ops)
               (case op
                 \+ (+ acc num)
                 \* (* acc num)
                 \| (cat-nums acc num)))))))

(defn determine-ops [ns]
  (let [n-ops (dec ns)
        permutations (combo/selections @ops n-ops)]
    (mapv #(apply str %) permutations)))

(defn check-formula [formula]
  (let [v (first formula)
        nums (rest formula)
        ops (determine-ops (count nums))]
    (some #(= v (eval-formula nums %)) ops)))

(defn solve [filename]
  (let [valid-formulae (->> filename
       parse-file
       (filter check-formula))]
    (reduce + (map first valid-formulae))))


