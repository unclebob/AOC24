(ns aoc2-spec
  (:require [aoc2 :refer :all]
            [speclj.core :refer :all]))

(def sample-data [[7 6 4 2 1]
                  [1 2 7 8 9]
                  [9 7 6 2 1]
                  [1 3 2 4 5]
                  [8 6 4 4 1]
                  [1 3 6 7 9]])
(describe "aoc2"
  (context "parse input"
    (it "parses a line"
      (should= [1 2 3 4 5] (parse-line "1  2  3  4  5")))

    (it "parses a file"
      (should= sample-data (parse-file "sample.txt")))
    )

  (context "problem 1"
    (it "is safe"
      (should-not (is-safe? []))
      (should-not (is-safe? [1]))
      (should-not (is-safe? [1 1]))
      (should-not (is-safe? [1 5]))
      (should-not (is-safe? [5 1]))
      (should-not (is-safe? [1 2 1]))
      (should-not (is-safe? [1 2 3 2 1]))

      (should (is-safe? [1 2]))
      (should (is-safe? [1 2 3]))
      (should (is-safe? [1 2 3 4]))
      (should (is-safe? [4 3 2 1]))

      (should= [true false false false false true]
               (map is-safe? sample-data)))

    (it "solves the sample problem"
      (should= 2 (count (filter is-safe? sample-data))))

    (it "solves the problem"
      (should= 321 (count (filter is-safe? (parse-file "input.txt")))))
    )

  (context "problem 2"
    (it "splits report into many with one missing"
      (should= [[2 3 4] [1 3 4] [1 2 4] [1 2 3]]
               (dampen-report [1 2 3 4])))

    (it "is-dampened-safe?"
      (should= [true false false true true true]
               (map is-dampened-safe? (parse-file "sample.txt"))))

    (it "solves the sample problem"
      (should= 4 (count (filter is-dampened-safe? (parse-file "sample.txt")))))

    (it "solves the problem"
      (should= 754 (count (filter is-dampened-safe? (parse-file "input.txt")))))
    )
  )
