(ns aoc11-spec
  (:require [aoc11 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc11"
  (context "parsing"
    (it "parses a line"
      (should= [1 2 3] (parse-line "1 2 3")))

    (it "parses a file"
      (should= [125 17] (parse-file "sample.txt")))
    )

  (context "part 1"
    (it "does blink rules"
      (should= 1 (blink-one 0))
      (should= [3 4] (blink-one 34))
      (should= [13 4] (blink-one 1304))
      (should= 2024 (blink-one 1)))

    (it "blinks some stones"
      (should= [253000 1 7] (blink-stones [125 17])))

    (it "blinks stones n times"
      (should= [2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2]
               (blink-stones-n-times 6 [125 17])))

    ;(it "solves the sample"
    ;	(should= 55312 (solve-1 "sample.txt")))
    ;
    ;(it "solves the input"
    ;	(should= 199982 (solve-1 "input.txt")))
    )

  (context "part 2"
    ;(it "solves the input"
    ;	(should= 0 (solve-2 "input.txt")))

    (it "blinks 1 a few times"
      (let [orig [773 79858 0 71 213357 2937 1 3998391]
            stones (blink-stones-n-times 25 orig)
            freqs-1 (frequencies stones)
            keys-2 (remove (set orig) (keys freqs-1))
            stones-2 (blink-stones-n-times 25 keys-2)
            freqs-2 (frequencies stones-2)
            keys-3 (remove (set keys-2) (keys freqs-2))
            freqs-3 (frequencies (blink-stones-n-times 25 keys-3))
            ]
        (should= 0 (count (keys freqs-3)))))
    )
  )

