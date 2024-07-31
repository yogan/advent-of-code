(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest dragon
  (testing "dragon works for 1"
    (is (= (aoc/dragon "1") (seq "100"))))
  (testing "dragon works for 0"
    (is (= (aoc/dragon "0") (seq "001"))))
  (testing "dragon works for 11111"
    (is (= (aoc/dragon "11111") (seq "11111000000"))))
  (testing "dragon works for 111100001010"
    (is (= (aoc/dragon "111100001010") (seq "1111000010100101011110000")))))

(deftest fill
  (testing "fill returns the data when the target length is reached"
    (is (= (aoc/fill "101" 3) "101")))
  (testing "fill truncates the data when there is is more data than needed"
    (is (= (aoc/fill "1010" 3) "101")))
  (testing "fill uses dragon to create more data when needed"
    (is (= (aoc/fill "1" 3) "100")))
  (testing "fill works for the example in the problem description"
    (is (= (aoc/fill "10000" 20) "10000011110010000111"))))

(deftest checksum
  (testing "checksum works for 110010110100"
    (is (= (aoc/checksum "110010110100") "100")))
  (testing "checksum works for 10000011110010000111"
    (is (= (aoc/checksum "10000011110010000111") "01100"))))

(deftest part1
  (testing "part1 works for the example in the problem description"
    (is (= (aoc/part1 "10000" 20) "01100"))))
