(ns aoc5-spec
  (:require [aoc5 :refer :all]
            [speclj.core :refer :all]))

(describe "aoc5"
  (context "parsing"
    (it "parses an ordering rule"
      (should= [47 53] (parse-rule "47|53")))

    (it "parses a page document list"
      (should= {75 0 29 1 13 2} (parse-document "75,29,13")))

    (it "parses a rules and documents"
      (should= {:rules [[47 53] [29 13]]
                :docs [{75 0 29 1 13 2}
                       {1 0 2 1 3 2}]}
               (parse-lines "47|53\n29|13\n\n75,29,13\n1,2,3\n")))

    (it "parses a file"
      (should= {:rules [[47 53] [97 13] [97 61] [97 47] [75 29] [61 13] [75 53] [29 13] [97 29] [53 29] [61 53] [97 53] [61 29] [47 13] [75 47] [97 75] [47 61] [75 61] [47 29] [75 13] [53 13]], :docs [{75 0, 47 1, 61 2, 53 3, 29 4} {97 0, 61 1, 53 2, 29 3, 13 4} {75 0, 29 1, 13 2} {75 0, 97 1, 47 2, 61 3, 53 4} {61 0, 13 1, 29 2} {97 0, 13 1, 75 2, 29 3, 47 4}]}
               (parse-file "sample.txt")))
    )

  (context "part 1"
    (it "determines validity of document"
      (let [rules [[47 53] [29 13]]
            doc {75 0 29 1 13 2}]
        (should (valid-doc? rules doc)))

      (let [rules [[47 53] [29 13]]
            doc {1 0 2 1 3 2 4 3}]
        (should (valid-doc? rules doc)))

      (let [rules [[47 53] [13 29]]
            doc {75 0 29 1 13 2}]
        (should-not (valid-doc? rules doc))))

    (it "gets middle value"
      (should= :y (middle-key {:x 0 :y 1 :z 2})))

    (it "solves the sample"
      (should= 143 (solve-1 "sample.txt")))

    (it "solves the problem"
      (should= 6051 (solve-1 "input.txt")))
    )

  (context "part 2"
    (it "fixes a document"
      (let [rules [[47 53] [29 13]]
            doc {75 0 29 1 13 2}]
        (should= [75 29 13] (fix-doc rules doc)))

      (let [rules [[47 53] [29 13]]
            doc {1 0 2 1 3 2 4 3}]
        (should= [1 2 3 4] (fix-doc rules doc)))

      (let [rules [[47 53] [13 29] [13 75]]
            doc {75 0 29 1 13 2}]
        (should (valid-doc? rules (fix-doc rules doc))))
      )

    (it "solves the sample"
      (should= 123 (solve-2 "sample.txt")))

    (it "solves the problem"
      (should= 5093 (solve-2 "input.txt")))
    )
  )

