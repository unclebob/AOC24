(ns aoc10
  (:require [clojure.string :as string]))

(defn parse-line [map line y]
  (loop [line line
         x 0
         map map]
    (if (empty? line)
      map
      (recur (rest line)
             (inc x)
             (assoc map [x y] (Character/digit ^char (first line) 10))))))

(defn parse-lines [lines]
  (loop [lines lines
         y 0
         map {}]
    (if (empty? lines)
      map
      (recur (rest lines)
             (inc y)
             (parse-line map (first lines) y)))))

(defn parse-file [filename]
  (-> filename slurp (string/split-lines) parse-lines))

(defn trail-heads [map]
  (mapv key (filter #(zero? (val %)) map)))

(defn add-delta [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn steps-up-from [start map]
  (let [height (get map start)]
    (for [delta [[0 1] [1 0] [0 -1] [-1 0]]
          :let [next (add-delta start delta)]
          :when (and (contains? map next)
                     (= (inc height) (get map next)))]
      next)))

(defn find-paths [start map]
  (let [ends []]
    (if (= 9 (get map start))
      (conj ends start)
      (let [steps (steps-up-from start map)
            paths (for [step steps]
                    (find-paths step map))
            ends (concat ends (partition 2 (flatten paths)))]
        ends))))

(defn count-paths [start map]
  (count (set (find-paths start map))))

(defn solve-1 [filename]
  (let [topo-map (parse-file filename)
        heads (trail-heads topo-map)]
    (reduce + (map #(count-paths % topo-map) heads))))