(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest process-line-test
  (testing "process-line should split the line by 'x'"
    (is (= (aoc/process-line "1x2x3") [1 2 3]))))

(deftest part1
  (testing "part1 returns the expected result"
    (is (= (aoc/part1 [[1 2 3] [1 1 1]]) (+ 6 1)))))

(deftest part2
  (testing "part2 returns the expected result"
    (is (= (aoc/part2 [[1 2 3] [1 1 1]]) (+ (+ 2 2 3 3 6 6) (* 6 1))))))