(ns aoc6
  (:require [clojure.set :as set]))

(defn parse-line [line y guard-map]
  (loop [line line
         guard-map guard-map
         x 0]
    (if (empty? line)
      guard-map
      (let [c (first line)]
        (condp = c
          \^ (recur (rest line)
                    (assoc guard-map :guard-pos [x y])
                    (inc x))

          \# (recur (rest line)
                    (assoc guard-map :blocks (conj (:blocks guard-map) [x y]))
                    (inc x))
          (recur (rest line) guard-map (inc x)))))))

(defn parse-input [lines]
  (loop [lines lines
         guard-map {:guard-pos nil
                    :guard-direction [0 -1]
                    :blocks []
                    :turns []}
         y 0]
    (if (empty? lines)
      guard-map
      (recur (rest lines)
             (parse-line (first lines) y guard-map)
             (inc y)))))

(defn parse-file [filename]
  (let [lines (-> filename
                  slurp
                  clojure.string/split-lines)
        guard-map (parse-input lines)]
    [(count (first lines))
     (count lines)
     guard-map]))

(defn turn-right [direction]
  (condp = direction
    [0 -1] [1 0]
    [1 0] [0 1]
    [0 1] [-1 0]
    [-1 0] [0 -1]))

(defn add-step [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn take-step [{:keys [guard-pos guard-direction blocks turns] :as guard-map}]
  (let [forward-pos (add-step guard-pos guard-direction)]
    (if (some #(= forward-pos %) blocks)
      (let [turned-direction (turn-right guard-direction)
            turned-pos (add-step guard-pos turned-direction)]
        (if (some #(= guard-pos %) turns)
          :loop
          (assoc guard-map :guard-direction turned-direction
                           :guard-pos turned-pos
                           :turns (conj turns guard-pos))))
      (assoc guard-map :guard-pos forward-pos))))

(defn out-of-bounds? [[x-bound y-bound] [x y]]
  (or (< x 0) (< y 0) (>= x x-bound) (>= y y-bound)))

(defn solve-map [x-bound y-bound guard-map]
  (loop [guard-map guard-map
         steps []]
    (if (out-of-bounds? [x-bound y-bound] (:guard-pos guard-map))
      {:steps steps :turns (:turns guard-map)}
      (let [step (take-step guard-map)]
        (if (= :loop step)
          :loop
          (recur step (conj steps (:guard-pos guard-map))))))))

(defn parse-and-solve-map [filename]
  (apply solve-map (parse-file filename)))

(defn solve-1 [filename]
  (count (set (:steps (parse-and-solve-map filename)))))

(defn find-crossings [steps]
  (loop [steps steps
         stepped #{}
         crossings []]
    (if (empty? steps)
      crossings
      (let [step (first steps)]
        (if (stepped step)
          (recur (rest steps) stepped (conj crossings step))
          (recur (rest steps) (conj stepped step) crossings))))))

(defn find-candidate-blocks [crossing]
  (for [delta [[0 -1] [1 0] [0 1] [-1 0]]]
    (add-step crossing delta)))

(defn solve-2 [filename]
  (let [[x-bound y-bound guard-map] (parse-file filename)
        steps (:steps (solve-map x-bound y-bound guard-map))]
     (loop [steps (set steps)
           loops []]
      (if (empty? steps)
        (count loops)
        (if (= :loop (solve-map x-bound y-bound (update guard-map :blocks conj (first steps))))
          (recur (rest steps) (conj loops (first steps)))
          (recur (rest steps) loops))))))



