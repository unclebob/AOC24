(ns aoc1-spec
  (:require [aoc1 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc1"
  (context "problem 1"
    (it "diff of sorted elements"
      (should= 11
               (diff-of-sorted-elements [3 4 2 1 3 3]
                                        [4 3 5 3 9 3])))

    (it "parses a line"
      (should= [1 2] (parse-line "1  2")))

    (it "parses many lines"
      (should= [[1 2] [3 4]] (parse-lines ["1  3" "2  4"])))

    (it "parses a file"
      (should= [[1 3] [2 4]]
               (parse-file "test.txt")))

    (it "solves the sample problem 1"
      (should= 11
               (solve-1 "sample.txt")))

    (it "solves problem 1"
      (should= 1197984
               (solve-1 "input.txt"))))

  (context "problem 2"
    (it "creates frequency list"
      (should= [[3 3] [4 1] [2 0] [1 0] [3 3] [3 3]]
               (create-frequency-list (parse-file "sample.txt"))))

    (it "solves the sample problem 2"
      (should= 31 (solve-2 "sample.txt")))

    (it "solves problem 2"
      (should= 660730
               (solve-2 "input.txt")))

    )
  )
