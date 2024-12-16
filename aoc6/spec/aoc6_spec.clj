(ns aoc6-spec
  (:require [aoc6 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc6"
  (context "parsing"
    (it "parses map"
      (should= {:guard-pos [1 1]
                :guard-direction [0 -1]
                :blocks [[0 0]]
                :turns []}
               (parse-input [
                             "#."
                             ".^"])))

    (it "parses file"
      (should= [10 10
                {:guard-pos [4 6],
                 :guard-direction [0 -1],
                 :blocks [[4 0] [9 1] [2 3] [7 4] [1 6] [8 7] [0 8] [6 9]]
                 :turns []}
                ]
               (parse-file "sample.txt")))
    )

  (context "part 1"
    (it "turns right"
      (should= [1 0] (turn-right [0 -1]))
      (should= [0 1] (turn-right [1 0]))
      (should= [-1 0] (turn-right [0 1]))
      (should= [0 -1] (turn-right [-1 0])))

    (it "takes a step with no blocks"
      (let [guard-map {:guard-pos [1 1]
                       :guard-direction [0 -1]
                       :blocks [[0 0]]
                       :turns []}]
        (should= {:guard-pos [1 0]
                  :guard-direction [0 -1]
                  :blocks [[0 0]]
                  :turns []}
                 (take-step guard-map))))

    (it "takes a step and turns right"
      (let [guard-map {:guard-pos [1 1]
                       :guard-direction [0 -1]
                       :blocks [[1 0]]
                       :turns []}]
        (should= {:guard-pos [2 1]
                  :guard-direction [1 0]
                  :blocks [[1 0]]
                  :turns [[1 1]]}
                 (take-step guard-map))))

    (it "solves sample"
      (should= 41 (solve-1 "sample.txt")))

    (it "solves input"
      (should= 4819 (solve-1 "input.txt")))
    )

  (context "part 2"
    (it "gathers turns"
      (should= [[4 1] [8 1] [8 6] [2 6] [2 4] [6 4] [6 8] [1 8] [1 7] [7 7]]
               (:turns (parse-and-solve-map "sample.txt"))))

    (it "detects loops"
      (should-not= :loop (take-step {:guard-pos [1 1]
                                     :guard-direction [0 -1]
                                     :blocks [[1 0]]
                                     :turns []}))
      (should= :loop (take-step {:guard-pos [1 1]
                                 :guard-direction [0 -1]
                                 :blocks [[1 0]]
                                 :turns [[9 9] [1 1]]})))

    (it "finds crossings"
      (should= [[1 1] [2 3]] (find-crossings [[1 1] [2 3] [1 1] [9 9] [2 3] [0 0]])))

    (it "solves sample"
      (should= 6 (solve-2 "sample.txt")))

    ;(it "solves input"
    ;  (should= 0 (solve-2 "input.txt")))
    )
  )


