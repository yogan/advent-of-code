(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]))

(defn map-parenthesis [line] (mapv #(if (= % \() 1 -1) line))

(defn find-basement [chars index floor]
  (if (or (empty? chars) (= floor -1))
    index
    (find-basement (rest chars) (inc index) (+ floor (first chars)))))

(defn part1 [nums] (apply + nums))
(defn part2 [nums] (find-basement nums 0 0))

(defn -main []
  (let [file "resources/input.txt"
        line (with-open [reader (io/reader file)]
               (first (line-seq reader)))
        nums (map-parenthesis line)]
    (println "Part 1:" (part1 nums))
    (println "Part 2:" (part2 nums))))
