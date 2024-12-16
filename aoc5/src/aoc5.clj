(ns aoc5
  (:require
    [clojure.set :as set]
    [clojure.string :as string]))

(defn parse-rule [rule]
  (mapv #(Integer/parseInt %) (string/split rule #"\|")))

(defn parse-document [doc]
  (let [doc-list (string/split doc #"\,")]
    (zipmap (mapv #(Integer/parseInt %) doc-list) (range))))

(defn parse-lines [input]
  (let [parts (string/split input #"\n\n")]
    {:rules (mapv parse-rule (string/split (first parts) #"\n"))
     :docs (mapv parse-document (string/split (second parts) #"\n"))}))

(defn parse-file [filename]
  (parse-lines (slurp filename)))

(defn valid-doc? [rules doc]
  (loop [rules rules]
    (if (empty? rules)
      true
      (let [[min-page max-page] (first rules)
            min-idx (get doc min-page)
            max-idx (get doc max-page)]
        (if (or (nil? min-idx)
                (nil? max-idx)
                (<= min-idx max-idx))
          (recur (rest rules))
          false)))))

(defn middle-key [doc]
  (let [n (count (keys doc))
        _ (assert (odd? n) "doc must have an odd number of keys")
        mid-idx (quot n 2)
        page (get (set/map-invert doc) mid-idx)]
    page))

(defn solve-1 [filename]
  (let [{:keys [rules docs]} (parse-file filename)
        valid-docs (filter #(valid-doc? rules %) docs)
        middle-pages (mapv middle-key valid-docs)]
    (reduce + middle-pages)))

(defn fix-doc [rules doc]
  (let [page-cmp (fn [p1 p2]
                   (some? (some #(= % [p1 p2]) rules)))
        idoc (set/map-invert doc)
        pages (for [i (range (count doc))]
                (get idoc i))
        ]
    (sort page-cmp pages)))

(defn middle-page [page-list]
  (let [n (count page-list)
        mid-idx (quot n 2)
        mid (nth page-list mid-idx)]
    mid))

(defn solve-2 [filename]
  (let [{:keys [rules docs]} (parse-file filename)
        invalid-docs (remove #(valid-doc? rules %) docs)
        fixed-docs (mapv #(fix-doc rules %) invalid-docs)
        middle-pages (mapv middle-page fixed-docs)]
    (reduce + middle-pages)))