(ns aoc13
  (:require [clojure.string :as string]))

(defn parse-button [line]
  (let [[_ button x y] (re-matches #"Button (\w): X([+-]\d+), Y([+-]\d+)" line)]
    {button [(Integer/parseInt x) (Integer/parseInt y)]}))

(defn parse-prize [line]
  (let [[_ x y] (re-matches #"Prize.*X=([+-]?\d+).*Y=([+-]?\d+)" line)]
    {"X" (Integer/parseInt x) "Y" (Integer/parseInt y)}))

(defn parse-machine [lines]
  (let [buttons (map parse-button (take 2 lines))
        prize (parse-prize (last lines))
        buttons (apply merge buttons)
        machine (merge buttons prize)]
    {:xa (first (machine "A")) :ya (second (machine "A"))
     :xb (first (machine "B")) :yb (second (machine "B"))
     :x (machine "X") :y (machine "Y")}))

(defn solve-machine
  ([machine]
   (solve-machine machine 0))

  ([{:keys [xa ya xb yb x y]} offset]
   (let [x (+ x offset)
         y (+ y offset)
         a (/ (- (* xb y) (* yb x)) (- (* xb ya) (* xa yb)))
         b (/ (- x (* xa a)) xb)]
     (if (and (int? a) (int? b)) (+ (* 3 a) b) 0)
     )))

(defn solve
  ([filename]
   (solve filename 0))

  ([filename offset]
   (loop [lines (-> filename slurp (string/split-lines))
          sum 0]
     (if (empty? lines)
       sum
       (let [tokens (-> (take 3 lines)
                         parse-machine
                         (solve-machine offset))]
         (recur (drop 4 lines) (+ sum tokens))))
     )))
