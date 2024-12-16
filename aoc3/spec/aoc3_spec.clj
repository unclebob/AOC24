(ns aoc3-spec
  (:require [aoc3 :refer :all]
            [speclj.core :refer :all]))
(def sample "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(describe "aoc3"
  (context "parse"
    (it "finds mul instructions"
      (should= [] (find-mul ""))
      (should= [] (find-mul "x"))

      (should= ["mul(1,2)"] (find-mul "mul(1,2)"))
      (should= ["mul(1,2)" "mul(3,4)"] (find-mul "mul(1,2) mul(3,4)"))
      (should= ["mul(2,4)"
                "mul(5,5)"
                "mul(11,8)"
                "mul(8,5)"] (find-mul sample)))
    (it "finds mul,do,and don't"
      (should= ["mul(2,4)"
                "do"
                "mul(3,9)"
                "don't"] (find-mul-do "mul(2,4)do()mul(3,9)don't")))

    (it "parses individual muls"
      (should= [2 4] (parse-mul "mul(2,4)")))

    (it "parses the file"
      (should= [[2 4] [5 5] [11 8] [8 5]] (parse-mul-file "sample.txt")))
    )

  (context "problem 1"
    (it "sovles the sample"
      (should= 161 (solve-1 "sample.txt")))
    (it "sovles the input"
      (should= 196826776 (solve-1 "input.txt")))
    )

  (context "problem 2"
    (it "prunes don'ts"
      (should= ["mul(1,2)"
                "mul(3,4)"] (prune-donts ["mul(1,2)" "don't" "do" "mul(3,4)"])))

    (it "solves the sample"
      (should= 48 (solve-2 "sample2.txt")))

    (it "solves the input"
      (should= nil (solve-2 "input.txt")))

    )
  )
