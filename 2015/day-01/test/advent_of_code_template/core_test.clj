(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest map-parenthesis-test
  (testing "map-parenthesis should map to a list of 1s and -1s"
    (is (= (aoc/map-parenthesis "(()(()(") [1 1 -1 1 1 -1 1]))))

(deftest part1
  (testing "part1 returns the expected result"
    (is (= (aoc/part1 "(()(()(") 3))))

(deftest part2
  (testing "part2 returns the expected result"
    (is (= (aoc/part2 "(()(()()") -1))))