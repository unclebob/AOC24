(ns aoc10-spec
  (:require [aoc10 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc10"
  (context "Parsing"
    (it "parses a line"
      (should= {[0 0] 0
                [1 0] 1
                [2 0] 2}
               (parse-line {} "012" 0)))

    (it "parses lines"
      (should= {[0 0] 0
                [1 0] 1
                [2 0] 2
                [0 1] 3
                [1 1] 4
                [2 1] 5}
               (parse-lines ["012" "345"])))
    )

  (context "part 1"
    (it "finds trail heads"
      (should= [[0 0] [0 1]]
               (trail-heads {[0 0] 0
                             [1 0] 1
                             [2 0] 2
                             [0 1] 0
                             [1 1] 4
                             [2 1] 5})))

    (it "finds steps up"
      (should= [[0 1] [1 0]]
               (steps-up-from [0 0]
                              {[0 0] 0
                               [1 0] 1
                               [2 0] 2
                               [0 1] 1
                               [1 1] 4
                               [2 1] 5})))

    (it "finds a path"
      (should= [[0 1] [1 0]]
               (find-paths [0 0]
                          {[0 0] 8
                           [1 0] 9
                           [0 1] 9})))

    (it "finds paths in the sample"
      (should= #{[4 3] [1 0] [5 4] [0 3] [4 5]}  (set (find-paths [2 0] (parse-file "sample.txt")))))

    (it "counts paths from a head in the sample"
      (should= 5 (count-paths [2 0] (parse-file "sample.txt"))))

    (it "solves the sample"
      (should= 36 (solve-1 "sample.txt")))

    (it "solves the input"
      (should= 789 (solve-1 "input.txt")))
    )

  )

