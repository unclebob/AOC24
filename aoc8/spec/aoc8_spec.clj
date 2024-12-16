(ns aoc8-spec
  (:require [aoc8 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc8"
  (context "parsing"
    (it "parses a line"
      (should= [{:freq \a :pos [2 3]}
                {:freq \b :pos [5 3]}]
               (parse-line "..a..b." 3)))

    (it "parses many lines"
      (should= [7 7 [{:freq \a :pos [2 0]}
                     {:freq \b :pos [5 0]}
                     {:freq \d :pos [4 1]}
                     {:freq \e :pos [6 4]}]]
               (parse-lines ["..a..b."
                             "....d.."
                             "......."
                             "......."
                             "......e"
                             "......."
                             "......."])))
    (it "parses a file"
      (should= [12 12
                [{:freq \0, :pos [8 1]}
                 {:freq \0, :pos [5 2]}
                 {:freq \0, :pos [7 3]}
                 {:freq \0, :pos [4 4]}
                 {:freq \A, :pos [6 5]}
                 {:freq \A, :pos [8 8]}
                 {:freq \A, :pos [9 9]}]]
               (parse-file "sample.txt")))

    )

  (context "part 1"
    (it "finds all antennae"
      (should= #{\0 \A} (get-antennae (parse-file "sample.txt"))))

    (it "gets all antennae of a certain frequency"
      (should= #{[6 5] [8 8] [9 9]}
               (get-antenna-positions (parse-file "sample.txt") \A)))

    (it "finds every pair of coordinates"
      (should= #{[[6 5] [9 9]] [[6 5] [8 8]] [[8 8] [9 9]]}
               (pairs-of [[6 5] [8 8] [9 9]])))

    (it "finds antinodes of a pair"
      (should= #{[3 1] [12 13]}
               (antinodes-of-pair [[6 5] [9 9]]))
      (should= #{[3 1] [12 13]}
               (antinodes-of-pair [[9 9] [6 5]]))
      )

    (it "identify antinodes that are in bounds"
      (should (in-bounds? [3 1] 4 4))
      (should (in-bounds? [0 0] 4 4))
      (should (in-bounds? [3 3] 4 4))

      (should-not (in-bounds? [4 4] 4 4))
      (should-not (in-bounds? [0 4] 4 4))
      (should-not (in-bounds? [4 0] 4 4))
      (should-not (in-bounds? [-1 0] 4 4))
      (should-not (in-bounds? [0 -1] 4 4)))

    (it "solves part 1"
      (should= 14 (solve-1 "sample.txt")))

    (it "solves part 1 for the input"
      (should= 379 (solve-1 "input.txt")))
    )

  (context "part 2"
    (it "finds delta of pair"
      (should= [1 1] (delta-of-pair [[0 0] [1 1]]))
      (should= [1 1] (delta-of-pair [[1 1] [0 0]]))
      (should= [1 1] (delta-of-pair [[0 0] [2 2]]))
      (should= [1 1] (delta-of-pair [[2 2] [0 0]]))
      (should= [1 1] (delta-of-pair [[1 1] [2 2]]))
      (should= [1 0] (delta-of-pair [[1 1] [2 1]]))
      (should= [0 -1] (delta-of-pair [[1 1] [1 0]]))
      (should= [0 2] (delta-of-pair [[1 0] [1 2]]))
      (should= [2 0] (delta-of-pair [[1 6] [3 6]]))
      (should= [2 3] (delta-of-pair [[1 1] [3 4]]))
      (should= [2 3] (delta-of-pair [[5 7] [1 1]]))
      (should= [2 1] (delta-of-pair [[1 1] [3 2]]))
      (should= [1 2] (delta-of-pair [[1 1] [2 3]])))

    (it "extends delta"
      (should= #{[0 0] [1 0] [2 0]} (extend-pair [[0 0] [1 0]] 3 4))
      (should= #{[0 0] [1 0] [2 0]} (extend-pair [[2 0] [1 0]] 3 4))
      (should= #{[0 0] [1 1] [2 2]} (extend-pair [[0 0] [1 1]] 3 4))
      (should= #{[3 0] [2 1] [1 2] [0 3]} (extend-pair [[3 0] [2 1]] 4 4))
      (should= #{[0 0] [0 1] [0 2] [0 3]} (extend-pair [[0 1] [0 2]] 4 4))
      )

    (it "solves part 2"
      (should= 34 (solve-2 "sample.txt")))

    (it "solves part 2 for the input"
      (should= 1339 (solve-2 "input.txt")))
    )
  )

