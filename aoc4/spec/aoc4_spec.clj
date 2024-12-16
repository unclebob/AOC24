(ns aoc4-spec
  (:require [aoc4 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc4"
  (context "part 1"
    (it "builds horizontal string"
      (should= "ab.bc" (build-horizontal-string ["ab" "bc"])))

    (it "transposes"
      (should= ["ac" "bd"] (transpose ["ab" "cd"])))

    (it "builds vertical string"
      (should= "ac.bd" (build-vertical-string ["ab" "cd"])))

    (it "builds LR diagonal string"
      (should= "a.cb.d" (build-lr-diagonal-string ["ab" "cd"]))
      (should= "a.db.gec.hf.i" (build-lr-diagonal-string ["abc" "def" "ghi"])))

    (it "builds the RL diagonal string"
      (should= "b.da.c" (build-rl-diagonal-string ["ab" "cd"])))

    (it "finds xmas in string"
      (should= ["XMAS"] (find-xmases "XMAS"))
      (should= ["SAMX"] (find-xmases "SAMX"))
      (should= ["XMAS" "SAMX"] (find-xmases "XMASAMX"))
      (should= ["XMAS" "SAMX"] (find-xmases "SDXMASEEBSAMXDDS")))

    (it "solves sample"
      (should= 18 (solve-1 "sample.txt")))

    (it "solves input"
      (should= 2483 (solve-1 "input.txt")))
    )

  (context "part2"
    (it "finds 3x3 block at coord"
      (should= ["abc" "def" "ghi"] (find-3x3-at-coord ["abc" "def" "ghi"] 0 0))
      (should= ["abc" "def" "ghi"] (find-3x3-at-coord ["xxxx" "xabc" "xdef" "xghi"] 1 1)))

    (it "finds all 3x3 blocks"
      (should= [["abc" "def" "ghi"]] (find-all-3x3-blocks ["abc" "def" "ghi"]))
      (should= [["abc" "efg" "ijk"]
                ["bcd" "fgh" "jkl"]
                ["efg" "ijk" "mno"]
                ["fgh" "jkl" "nop"]] (find-all-3x3-blocks ["abcd" "efgh" "ijkl" "mnop"])))

    (it "finds x-mas in 3x3"
      (should (find-x-mas ["M.M" ".A." "S.S"]))
      (should (find-x-mas ["S.S" ".A." "M.M"]))
      (should (find-x-mas ["M.S" ".A." "M.S"]))
      (should (find-x-mas ["S.M" ".A." "S.M"]))


      (should-not (find-x-mas ["M.M" ".A." "M.M"]))
      (should-not (find-x-mas ["S.S" ".A." "S.S"]))
      (should-not (find-x-mas ["M.S" ".A." "S.M"]))
      (should-not (find-x-mas ["S.M" ".A." "M.S"]))
      (should-not (find-x-mas ["S.M" ".X." "S.M"])))

    (it "solves sample"
      (should= 9 (solve-2 "sample.txt")))

    (it "solves input"
      (should= 1925 (solve-2 "input.txt")))
    )
  )

