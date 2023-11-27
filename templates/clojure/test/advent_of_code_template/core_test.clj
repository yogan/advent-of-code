(ns advent-of-code-template.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent-of-code-template.core :as aoc]))

(deftest process-line-test
  (testing "process-line should split the line by 'x'"
    (is (= [1 2 3] (aoc/process-line "1x2x3")))))