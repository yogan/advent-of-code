(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest parse-draw-test-all-colors
  (testing "parse-draw should parse a draw with all colors"
    (let [draws (aoc/parse-draw "3 blue, 4 red, 23 green")]
      (is (= draws {:red 4 :green 23 :blue 3})))))

(deftest parse-draw-test-two-colors
  (testing "parse-draw should parse a draw with two colors"
    (let [draws (aoc/parse-draw "3 blue, 23 green")]
      (is (= draws {:red 0 :green 23 :blue 3})))))

(deftest parse-draw-test-no-colors
  (testing "parse-draw should parse a draw with no colors"
      (is (= (aoc/parse-draw "xxx") {:red 0 :green 0 :blue 0}))))

(deftest parse-line-test-game-id
  (testing "parse-line should parse the game id and draws"
    (let [line "Game 23: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
          result (aoc/parse-line line)]
      (is (= result [23 (list {:blue 3 :red 4 :green 0}
                              {:blue 6 :red 1 :green 2}
                              {:blue 0 :red 0 :green 2})])))))

(deftest is-draw-possible-all-under-limits
  (testing "is-draw-possible returns true when all colors are under their limits"
    (let [draw {:red 1 :green 2 :blue 3}]
      (is (= (aoc/is-draw-possible draw) true)))))

(deftest is-draw-possible-all-at-limits
  (testing "is-draw-possible returns true when all colors are at their limits"
    (let [draw {:red 12 :green 13 :blue 14}]
      (is (= (aoc/is-draw-possible draw) true)))))

(deftest is-draw-possible-red-over-limit
  (testing "is-draw-possible returns true when red is over the limit"
    (let [draw {:red 13 :green 13 :blue 14}]
      (is (= (aoc/is-draw-possible draw) false)))))

(deftest is-draw-possible-red-over-limit
  (testing "is-draw-possible returns true when blue is over the limit"
    (let [draw {:red 1 :green 13 :blue 15}]
      (is (= (aoc/is-draw-possible draw) false)))))

(defonce sample-lines
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(deftest possible-game-ids-sample
  (testing "possible-game-ids returns the ids from the sample"
    (let [games (map aoc/parse-line sample-lines)]
      (is (= (aoc/possible-game-ids games) [1 2 5])))))

(deftest part1
  (testing "part1 returns the expected result for the sample"
    (let [games (map aoc/parse-line sample-lines)]
      (is (= (aoc/part1 games) 8)))))

(deftest find-required-cubes-sample-game-1
  (testing "find-required-cubes returns the expected result for first example game"
    (let [draws (list {:red 4 :green 0 :blue 3}
                      {:red 1 :green 2 :blue 6}
                      {:red 0 :green 2 :blue 0})]
      (is (= (aoc/find-required-cubes draws)
             {:red 4 :green 2 :blue 6})))))

(deftest cubes-per-game-sample
  (testing "cubes-per-game returns the expected result for the sample"
    (let [games (map aoc/parse-line sample-lines)
          draw-list (map second games)]
      (is (= (aoc/cubes-per-game draw-list)
             (list {:red 4  :green 2  :blue 6}
                   {:red 1  :green 3  :blue 4}
                   {:red 20 :green 13 :blue 6}
                   {:red 14 :green 3  :blue 15}
                   {:red 6  :green 3  :blue 2}))))))

(deftest game-power
  (testing "game-power returns the expected result for the first sample game"
      (is (= (aoc/game-power {:red 4 :green 2 :blue 6}) 48))))

(deftest part2
  (testing "part2 returns the expected result for the sample"
    (let [games (map aoc/parse-line sample-lines)]
    (is (= (aoc/part2 games) 2286)))))
