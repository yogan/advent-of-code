(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]))

(defn map-parenthesis [line] (mapv #(if (= % \() 1 -1) line))

(defn part1 [line] (apply + (map-parenthesis line)))

(defn part2 [line] -1)

(defn -main []
  (let [file "resources/input.txt"
        line (with-open [reader (io/reader file)]
               (first (line-seq reader)))]
    (println "Part 1:" (part1 line))
    (println "Part 2:" (part2 line))))
