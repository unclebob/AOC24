(ns aoc8
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn isAntenna? [c]
  (not= \. c))

(defn parse-line [line y]
  (for [x (range (count line))
        :when (isAntenna? (nth line x))]
    {:freq (nth line x) :pos [x y]}))

(defn parse-lines [lines]
  (let [w (count (first lines))
        h (count lines)]
    [w h (flatten (for [y (range h)]
                    (parse-line (nth lines y) y)))]))

(defn parse-file [filename]
  (-> filename slurp string/split-lines parse-lines))

(defn get-antennae [anntenna-map]
  (set (map :freq (last anntenna-map))))

(defn get-antenna-positions [anntenna-map freq]
  (set (map :pos (filter #(= freq (:freq %)) (last anntenna-map)))))

(defn pairs-of [positions]
  (set (combo/combinations positions 2)))

(defn antinodes-of-pair [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (set [[(- x1 dx) (- y1 dy)] [(+ x2 dx) (+ y2 dy)]])))

(defn in-bounds? [[x y] w h]
  (and (<= 0 x (- w 1))
       (<= 0 y (- h 1))))

(defn get-antinodes [freq [w h _ :as ant-map]]
  (let [positions (get-antenna-positions ant-map freq)
        pairs (pairs-of positions)
        antipodes (mapcat antinodes-of-pair pairs)]
    (filter #(in-bounds? % w h) antipodes)))

(defn solve-1 [filename]
  (let [ant-map (parse-file filename)
        ant-freqs (get-antennae ant-map)]
    (loop [ant-freqs ant-freqs
           antinodes []]
      (if (empty? ant-freqs)
        (count (set antinodes))
        (recur (rest ant-freqs)
               (concat antinodes (get-antinodes (first ant-freqs) ant-map)))))))

(defn delta-of-pair [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        m (if (zero? dx)
            :inf
            (/ dy dx))]
    (cond
      (zero? dy)
      [dx 0]
      (= clojure.lang.Ratio (type m))
      [(denominator m) (numerator m)]
      (= :inf m)
      [0 dy]
      :else
      [1 m])))

(defn add-delta [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn subtract-delta [[x y] [dx dy]]
  [(- x dx) (- y dy)])

(defn extend-pair [pair w h]
  (let [[dx dy] (delta-of-pair pair)
        extend-p (fn [p f]
                   (loop [p p
                          ps []]
                     (if (not (in-bounds? p w h))
                       ps
                       (recur (f p [dx dy])
                              (conj ps p)))))
        right-ps-1 (extend-p (first pair) add-delta)
        left-ps-1 (extend-p (first pair) subtract-delta)
        right-ps-2 (extend-p (second pair) add-delta)
        left-ps-2 (extend-p (second pair) subtract-delta)]
    (set (concat right-ps-1 left-ps-1 right-ps-2 left-ps-2))))

(defn get-harmonic-antinodes [freq [w h _ :as ant-map]]
  (let [positions (get-antenna-positions ant-map freq)
        pairs (pairs-of positions)
        antinodes (mapcat #(extend-pair % w h) pairs)]
    antinodes))

(defn solve-2 [filename]
  (let [ant-map (parse-file filename)
        ant-freqs (get-antennae ant-map)]
    (loop [ant-freqs ant-freqs
           antinodes []]
      (if (empty? ant-freqs)
        (count (set antinodes))
        (recur (rest ant-freqs)
               (concat antinodes (get-harmonic-antinodes (first ant-freqs) ant-map)))))))
