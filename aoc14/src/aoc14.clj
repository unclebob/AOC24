(ns aoc14
  (:require [clojure.string :as string]))

(def w (atom 0))
(def h (atom 0))

(defn parse-line [line]
  (let [[_ x y dx dy] (re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" line)]
    [[(Integer/parseInt x) (Integer/parseInt y)]
     [(Integer/parseInt dx) (Integer/parseInt dy)]]))

(defn parse-lines [lines]
  (map parse-line lines))

(defn step-robot [[[x y] [dx dy]]]
  [[(mod (+ x dx) @w) (mod (+ y dy) @h)] [dx dy]])

(defn step-robots
  ([robots _]
   (step-robots robots))

  ([robots]
   (map step-robot robots)))

(defn quadrants []
  (let [[[x1 y1] [x2 y2] :as q1] [[0 0] [(dec (quot @w 2)) (dec (quot @h 2))]]
        [[x3 y3] [x4 y4] :as q2] [[(inc (quot @w 2)) 0] [(dec @w) y2]]
        [[x5 y5] [x6 y6] :as q3] [[0 (inc (quot @h 2))] [x2 (dec @h)]]
        [[x7 y7] [x8 y8] :as q4] [[x3 y5] [x4 y6]]]

    [q1 q2 q3 q4]))

(defn in-quadrant? [[[x1 y1] [x2 y2]] [[x y] _]]
  (and (<= x1 x x2) (<= y1 y y2)))

(defn count-in-quadrant [robots quadrant]
  (count (filter #(in-quadrant? quadrant %) robots)))

(defn mean [a] (/ (reduce + a) (count a)))
(defn square [a] (* a a))

(defn stddev [a]
  (let [mu (mean a)
        diffs (map #(- % mu) a)
        squared-diffs (vec (map square diffs))
        sum-of-diffs (reduce + squared-diffs)]
    (Math/sqrt (/ sum-of-diffs (count a)))))

(defn possible-tree? [counts]
  (> (stddev counts) 65))

(defn solve-1 [filename seconds]
  (let [robots (-> filename slurp string/split-lines parse-lines)
        qs (quadrants)
        stepped-robots (reduce step-robots robots (range seconds))
        counts (map #(count-in-quadrant stepped-robots %) qs)]
    (reduce * counts)))

(defn display [robots]
  (let [coords (set (map first robots))
        chars (for [y (range @h) x (range @w)]
                (if (contains? coords [x y]) "*" " "))
        lines (mapv #(apply str %) (partition @w chars))]
    (doseq [line lines]
      (println line))
    ))

(defn solve-2 [filename seconds]
  (let [robots (-> filename slurp string/split-lines parse-lines)
        stepped-robots (reduce step-robots robots (range seconds))]
    stepped-robots))


(defn step-and-count [robots n]
  (let [stepped-robots (step-robots robots)
        counts (map #(count-in-quadrant stepped-robots %) (quadrants))]
    (when (possible-tree? counts)
      (display stepped-robots)
      (prn (inc n) (stddev counts) counts))
    stepped-robots))


