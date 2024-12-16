(ns aoc12
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (map #(vector %1 %2) (range) line))

(defn parse-lines [lines]
  (let [w (count (first lines))
        h (count lines)]
    (loop [lines lines
           result {}
           y 0]
      (if (empty? lines)
        [w h result]
        (recur (rest lines)
               (reduce #(assoc %1 [(first %2) y] (second %2))
                       result (parse-line (first lines)))
               (inc y))))))

(defn parse-file [filename]
  (-> filename slurp string/split-lines parse-lines))

(defn neighbors [point w h]
  (let [[x y] point]
    (set
      (filter (fn [[x y]]
                (and (>= x 0) (< x w) (>= y 0) (< y h)))
              [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))))

(defn find-region
  ([start grid w h]
   (find-region start grid #{}
                (get grid start)
                w h))

  ([point grid region veg w h]
   (let [region (conj region point)
         all-neighbors (neighbors point w h)
         veg-neighbors (filter #(= veg (get grid %)) all-neighbors)
         neighbors (remove #(contains? region %) veg-neighbors)]
     (if (empty? neighbors)
       region
       (reduce (fn [region p]
                 (find-region p grid region veg w h))
               region neighbors)))))

(defn find-all-regions [grid w h]
  (loop [points (keys grid)
         regions []]
    (if (empty? points)
      regions
      (let [point (first points)
            region (find-region point grid w h)]
        (recur (remove region points)
               (conj regions region))))))

(defn area-perimeter [region grid w h]
  (let [area (count region)
        veg (get grid (first region))]
    (loop [points region
           perimeter 0]
      (if (empty? points)
        [area perimeter]
        (let [point (first points)
              neighbors (neighbors point w h)
              brothers (filter #(= (get grid %) veg) neighbors)
              edges (- 4 (count brothers))]
          (recur (rest points)
                 (+ perimeter edges)))))))

(defn solve-1 [filename]
  (let [[w h grid] (parse-file filename)
        regions (find-all-regions grid w h)
        stats (map #(area-perimeter % grid w h) regions)
        products (map (fn [[a p]] (* a p)) stats)]
    (reduce + products)))



