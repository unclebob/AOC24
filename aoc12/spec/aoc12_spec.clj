(ns aoc12-spec
  (:require [aoc12 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc12"
  (context "parses"
    (it "parses a single line"
      (should= [[0 \A] [1 \B] [2 \C] [3 \D]] (parse-line "ABCD")))

    (it "parses multiple lines"
      (should= [4 2
                {[0 0] \A
                 [1 0] \B
                 [2 0] \C
                 [3 0] \D
                 [0 1] \E
                 [1 1] \F
                 [2 1] \G
                 [3 1] \H}] (parse-lines ["ABCD" "EFGH"])))
    )

  (context "part 1"
    (it "finds neighbors"
      (should= #{[0 1] [1 0]} (neighbors [0 0] 3 3))
      (should= #{[0 0] [1 1] [2 0]} (neighbors [1 0] 3 3)))

    (it "finds a region starting at a point"
      (should= #{[0 0]} (find-region [0 0] {[0 0] \A} 3 3)))

    (it "finds complex region from sample"
      (let [[w h grid] (parse-file "sample.txt")]
        (should= #{[2 2] [0 0] [1 0] [2 3] [1 1] [4 2] [3 0] [2 0] [3 1] [2 1] [3 2] [0 1]}
                 (find-region [0 0] grid w h))))

    (it "finds all regions in sample"
      (let [[w h grid] (parse-file "sample.txt")]
        (should= [#{[8 8] [8 7] [9 8] [8 9] [8 6] [7 8] [9 6] [9 9] [8 5] [7 9] [9 7] [9 5] [9 4]}
                  #{[7 6] [7 7] [6 7] [5 4] [6 3] [6 6] [6 5] [6 4] [6 8] [6 9] [7 5]}
                  #{[7 1] [4 3] [3 3] [5 3] [5 2] [8 1] [6 1] [5 6] [5 5] [4 5] [7 0] [4 4] [6 2] [6 0]}
                  #{[2 2] [0 0] [1 0] [2 3] [1 1] [4 2] [3 0] [2 0] [3 1] [2 1] [3 2] [0 1]}
                  #{[3 9] [2 8] [2 5] [4 7] [4 6] [5 7] [1 8] [1 7] [5 8] [2 7] [3 6] [3 8] [3 7] [2 6]}
                  #{[8 4] [7 2] [8 3] [7 3] [9 0] [9 3] [8 0] [8 2] [9 2] [9 1]}
                  #{[7 4]}
                  #{[0 6] [0 5] [3 4] [1 4] [1 3] [1 5] [0 3] [2 4] [0 2] [0 4] [1 6] [1 2] [3 5]}
                  #{[1 9] [2 9] [0 9] [0 7] [0 8]}
                  #{[4 9] [4 8] [5 9]}
                  #{[4 1] [5 1] [5 0] [4 0]}]
                 (find-all-regions grid w h))))

    (it "finds area and perimeter of a region"
      (let [[w h grid] (parse-file "sample.txt")]
        (should= [10 18] (area-perimeter
                           #{[8 4] [7 2] [8 3] [7 3] [9 0] [9 3] [8 0] [8 2] [9 2] [9 1]}
                           grid w h))))

    (it "solves sample"
      (should= 1930 (solve-1 "sample.txt")))

    (it "solves input"
      (should= 1415378 (solve-1 "input.txt")))
    )
  )

