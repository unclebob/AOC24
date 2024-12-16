(ns aoc9-spec
  (:require [aoc9 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc9"
  (context "parsing"
    (it "parses a disk map"
      (should= [0 0 0 :f :f]
               (parse-disk-map "32"))
      (should= [0 0 :f :f :f
                1 1 1 :f :f :f
                2 :f :f :f
                3 3 3 :f
                4 4 :f
                5 5 5 5 :f
                6 6 6 6 :f
                7 7 7 :f
                8 8 8 8
                9 9]
               (parse-disk-map "2333133121414131402")))
    )

  (context "part 1"
    (it "finds free blocks"
      (should= 3 (find-free-blocks-after 0 [0 0 0 :f :f])))

    (it "compresses the map"
      (should= [0 0 1 1 1 :f :f :f]
               (compress-disk (parse-disk-map "233")))
      (should= [0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6 :f :f :f :f :f :f :f :f :f :f :f :f :f :f]
               (compress-disk (parse-disk-map "2333133121414131402"))))

    (it "Solves sample"
      (should= 1928 (solve-1 "sample.txt")))

    (it "solves input"
      (should= 6367087064415 (solve-1 "input.txt")))
    )
  )

