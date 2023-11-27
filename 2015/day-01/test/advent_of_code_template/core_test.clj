(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest map-parenthesis-test
  (testing "map-parenthesis should map to a list of 1s and -1s"
    (is (= (aoc/map-parenthesis "(()(()(") [1 1 -1 1 1 -1 1]))))

(deftest part1
  (testing "part1 returns the expected result"
    (let [nums (aoc/map-parenthesis "(()(()(")]
      (is (= (aoc/part1 nums) 3)))))

(deftest part2-ex1
  (testing "part2 returns the expected result for the first example"
    (let [nums (aoc/map-parenthesis ")")]
      (is (= (aoc/part2 nums) 1)))))

(deftest part2-ex1-ext
  (testing "part2 returns the expected result for the first example with extra input"
    (let [nums (aoc/map-parenthesis "))))(((")]
      (is (= (aoc/part2 nums) 1)))))

(deftest part2-ex2
  (testing "part2 returns the expected result for the second example"
    (let [nums (aoc/map-parenthesis "()())")]
      (is (= (aoc/part2 nums) 5)))))

(deftest part2-ex2-ext
  (testing "part2 returns the expected result for the second example with extra input"
    (let [nums (aoc/map-parenthesis "()())))((")]
      (is (= (aoc/part2 nums) 5)))))
