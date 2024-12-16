(ns aoc4
  (:require [clojure.string :as string]))

(defn build-horizontal-string [lines]
  (string/join "." lines))

(defn transpose [lines]
  (let [rotated-chars (apply mapv vector lines)]
    (map #(apply str %) rotated-chars))
  )

(defn build-vertical-string [lines]
  (-> lines transpose build-horizontal-string))

(defn build-lr-diagonal-string [lines]
  (let [width (count (first lines))
        height (count lines)
        remove-invalids (fn [coords]
                          (->> coords
                               (remove #(neg? (second %)))
                               (remove #(>= (second %) height))))
        build-from-coords (fn [coords]
                            (->> coords
                                 (map #(get-in lines (reverse %)))
                                 (apply str)))
        d-rows (loop [x 0 y 0 rows []]
                 (if (> y (+ height width -2))
                   rows
                   (let [coords
                         (loop [x x y y coords []]
                           (if (>= x width)
                             coords
                             (recur (inc x) (dec y) (conj coords [x y]))))]
                     (recur 0 (inc y) (conj rows coords)))))
        d-rows (map remove-invalids d-rows)
        d-strings (map build-from-coords d-rows)]
    (string/join "." d-strings)))

(defn build-rl-diagonal-string [lines]
  (let [reversed-lines (mapv string/reverse lines)]
    (build-lr-diagonal-string reversed-lines)))

(defn find-xmases [s]
  (let [xmases (re-seq #"XMAS" s)
        samxes (re-seq #"SAMX" s)]
    (concat xmases samxes)))

(defn solve-1 [filename]
  (let [lines (-> filename slurp (string/split-lines))
        h-lines (build-horizontal-string lines)
        v-lines (build-vertical-string lines)
        lr-lines (build-lr-diagonal-string lines)
        rl-lines (build-rl-diagonal-string lines)
        all-lines (string/join "." [h-lines v-lines lr-lines rl-lines])
        xmases (find-xmases all-lines)]
    (count xmases)
    ))

(defn find-3x3-at-coord [lines x y]
  (let [width (count (first lines))
        height (count lines)
        coords (for [x (range x (+ x 3))
                     y (range y (+ y 3))
                     :when (and (< x width) (< y height))]
                 [x y])
        chars (map #(get-in lines %) coords)
        rows (partition 3 3 chars)]
    (map #(apply str %) rows)))

(defn find-all-3x3-blocks [lines]
  (let [width (count (first lines))
        height (count lines)]
    (for [x (range width)
          y (range height)
          :when (and (<= (+ x 3) width) (<= (+ y 3) height))]
      (find-3x3-at-coord lines x y))))

(defn swap-sm [c]
  (condp = c
    \M \S
    \S \M
    nil))

(defn find-x-mas [[[a _ c] [_ e _] [g _ i]]]
  (let [xi (swap-sm a)
        xg (swap-sm c)]
    (and (= e \A) (= i xi) (= g xg))))

(defn solve-2 [filename]
  (let [lines (-> filename slurp (string/split-lines))
        blocks (find-all-3x3-blocks lines)
        x-mases (filter find-x-mas blocks)]
    (count x-mases)))



