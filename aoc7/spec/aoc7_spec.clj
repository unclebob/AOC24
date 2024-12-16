(ns aoc7-spec
	(:require [speclj.core :refer :all]
						[aoc7 :refer :all]))

(describe "aoc7"
	(context "parsing"
		(before-all (reset! ops [\+ \*]))
		(it "parses a line"
			(should= [1 2 3 4] (parse-line "1: 2 3 4")))

		(it "parses many lines"
			(should= [[1 2 3 4] [5 6 7 8]]
							 (parse-lines ["1: 2 3 4" "5: 6 7 8"])))

		(it "parses a file"
			(should= [[190 10 19]
								[3267 81 40 27]
								[83 17 5]
								[156 15 6]
								[7290 6 8 6 15]
								[161011 16 10 13]
								[192 17 8 14]
								[21037 9 7 18 13]
								[292 11 6 16 20]]
							 (parse-file "sample.txt")))
		)

	(context "part 1"
		(it "evaluates the formula"
			(should= 13 (eval-formula [1 2 3 4] "+*+")))

		(it "determines possible operations"
			(should= ["+" "*"] (determine-ops 2))
			(should= ["++" "+*" "*+" "**"] (determine-ops 3)))

		(it "checks a formula"
			(should (check-formula [190 10 19]))
			(should (check-formula [3267 81 40 27]))
			(should-not (check-formula [83 17 5]))
			(should-not (check-formula [156 15 6])))

		(it "solves the sample"
			(should= 3749 (solve "sample.txt")))

		(it "solves the problem"
			(should= 1298103531759 (solve "input.txt")))
		)

	(context "part 2"
		(before-all (reset! ops [\+ \* \|]))
		(it "evaluates the formula"
			(should= 94 (eval-formula [1 2 3 4] "+*|")))

		(it "solves the sample"
			(should= 11387 (solve "sample.txt")))

		(it "solves the problem"
			(should= nil (solve "input.txt")))
		)
	)

