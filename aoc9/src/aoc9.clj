(ns aoc9)

(defn ctoi [c]
  (Integer/parseInt (str c)))

(defn parse-disk-map [map-string]
  (loop [map-string map-string
         block-map []
         id 0
         free false]
    (if (empty? map-string)
      (if free
        block-map
        block-map)
      (let [c (first map-string)]
        (if free
          (if (= \0 c)
            (recur (rest map-string)
                   block-map
                   id
                   false)
            (recur (rest map-string)
                   (vec (concat block-map (repeat (ctoi c) :f)))
                   id
                   false))
          (recur (rest map-string)
                 (vec (concat block-map
                              (repeat (ctoi c)
                                      id)))
                 (inc id)
                 true))))))

(defn find-free-blocks-after [pos disk-map]
  (loop [pos pos]
    (if (= :f (nth disk-map pos))
      pos
      (recur (inc pos)))
  ))

(defn swap-block [disk-map pos1 pos2]
  (let [block1 (nth disk-map pos1)
        block2 (nth disk-map pos2)]
    (assoc disk-map pos1 block2
           pos2 block1)))

(defn compress-disk [disk-map]
  (loop [disk-map disk-map
         first-free-pos (find-free-blocks-after 0 disk-map)
         next-block (dec (count disk-map))]
    (if (>= first-free-pos next-block)
      disk-map
      (if (= :f (nth disk-map next-block))
        (recur disk-map
               first-free-pos
               (dec next-block))
        (recur (swap-block disk-map first-free-pos next-block)
               (find-free-blocks-after (inc first-free-pos) disk-map)
               (dec next-block))))))

(defn evaluate-map [disk-map]
  (let [blocks (remove #{:f} disk-map)
        products (map #(* %1 %2) (range (count blocks)) blocks)]
    (reduce + products)))


(defn solve-1 [filename]
  (let [disk-map (parse-disk-map (slurp filename))
        compressed-map (compress-disk disk-map)]
    (evaluate-map compressed-map)
    ))
