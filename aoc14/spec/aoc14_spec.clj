(ns aoc14-spec
  (:require [aoc14 :refer :all]
            [clojure.string :as string]
            [speclj.core :refer :all]))

(describe "aoc14"
  (context "parsing"
    (it "parses a line"
      (should= [[0 4] [3 -3]]
               (parse-line "p=0,4 v=3,-3")))

    (it "parses many lines"
      (should= [[[0 4] [3 -3]] [[1 2] [3 4]]]
               (parse-lines ["p=0,4 v=3,-3" "p=1,2 v=3,4"])))
    )

  (context "part 1"
    (before-all (reset! w 11) (reset! h 7))
    (it "takes a step"
      (should= [[[1 1] [1 1]] [[2 2] [1 1]]]
               (step-robots [[[0 0] [1 1]] [[1 1] [1 1]]])))

    (it "wraps around"
      (should= [[[0 0] [1 1]] [[10 6] [-1 -1]]]
               (step-robots [[[10 6] [1 1]] [[0 0] [-1 -1]]])))

    (it "finds quadrants"
      (should= [[[0 0] [4 2]]
                [[6 0] [10 2]]
                [[0 4] [4 6]]
                [[6 4] [10 6]]]
               (quadrants)))

    (it "counts in quadrant"
      (should= 2 (count-in-quadrant [[[0 0] nil]
                                     [[1 1] nil]
                                     [[2 2] nil]
                                     [[-1 -1] nil]]
                                    [[0 0] [1 1]])))

    (it "solves sample"
      (should= 12 (solve-1 "sample.txt" 100)))

    (it "solves input"
      (reset! w 101) (reset! h 103)
      (should= 224357412 (solve-1 "input.txt" 100))))

  (context "part 2"
    ;(it "displays robots"
    ;  (should= nil (display (-> "input.txt" slurp string/split-lines parse-lines))))

    (it "solves input"
      (reset! w 101) (reset! h 103)
      (let [robots (-> "input.txt" slurp string/split-lines parse-lines)]
        (reduce step-and-count robots (range 10000))))

    )
  )

