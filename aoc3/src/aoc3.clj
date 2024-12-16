(ns aoc3)

(defn find-mul [s]
  (let [muls (re-seq #"mul\(\d+,\d+\)" s)]
    (if (empty? muls) [] muls)))

(defn find-mul-do [s]
  (let [muls (re-seq #"mul\(\d+,\d+\)|don't|do" s)]
    (if (empty? muls) [] muls)))

(defn parse-mul [s]
  (let [[_ a b] (re-find #"mul\((\d+),(\d+)\)" s)]
    [(Integer/parseInt a) (Integer/parseInt b)]))

(defn parse-mul-file [filename]
  (->> (slurp filename)
       (find-mul)
       (map parse-mul)))

(defn solve-1 [file]
  (let [mul-args (parse-mul-file file)]
    (reduce + (map (fn [[a b]] (* a b)) mul-args))))

(defn prune-donts [s]
  (loop [s s doing? true result []]
    (if (empty? s)
      result
      (let [head (first s)
            tail (rest s)]
        (cond
          (= head "don't") (recur tail false result)
          (= head "do") (recur tail true result)
          doing? (recur tail true (conj result head))
          :else (recur tail false result))))))

(defn solve-2 [file]
  (let [input (find-mul-do (slurp file))
        _ (prn 'input input)
        pruned (prune-donts input)
        _ (prn 'pruned pruned)
        mul-args (map parse-mul pruned)]
    (reduce + (map (fn [[a b]] (* a b)) mul-args))))