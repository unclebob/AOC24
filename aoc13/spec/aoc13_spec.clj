(ns aoc13-spec
  (:require [aoc13 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc13"
  (context "parsing"
    (it "parses a button"
      (should= {"A" [94 34]}
               (parse-button "Button A: X+94, Y+34")))

    (it "parses a prize"
      (should= {"X" 8400 "Y" 5400}
               (parse-prize "Prize: X=8400, Y=5400")))

    (it "parses a machine"
      (should= {:xa 94 :ya 34 :xb 22 :yb 67 :x 8400 :y 5400}
               (parse-machine ["Button A: X+94, Y+34"
                               "Button B: X+22, Y+67"
                               "Prize: X=8400, Y=5400"])))
    )

  (context "part 1"
    (it "solves a machine"
      (should= 280
               (solve-machine (parse-machine
                                ["Button A: X+94, Y+34"
                                 "Button B: X+22, Y+67"
                                 "Prize: X=8400, Y=5400"]))))
    (it "identifies no solution"
      (should= 0
               (solve-machine (parse-machine
                                ["Button A: X+26, Y+66"
                                 "Button B: X+67, Y+21"
                                 "Prize: X=12748, Y=12176"]))))

    (it "solves the sample"
      (should= 480 (solve "sample.txt")))

    (it "solves the input"
      (should= 34787 (solve "input.txt")))
    )

  (context "part 2"
    (it "solves sample"
      (should= 875318608908
               (solve "sample.txt" 10000000000000)))

    (it "solves input"
      (should= 0 (solve "input.txt" 10000000000000))))

  )

(describe "coverage diag"
  (it "checks direct linking"
    (should= false (get (System/getProperties) "clojure.compiler.direct-linking"))))

