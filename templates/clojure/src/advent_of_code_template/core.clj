(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(defn process-line [line]
  (mapv #(Long/parseLong %) (cstr/split line #"x")))

(defn surface-area [numbers]
  (let [[l w h] numbers]
    (+ (* 2 l w) (* 2 w h) (* 2 h l))))

(defn volume [numbers] (apply * numbers))

(defn part1 [dimensions]
  (apply + (map volume dimensions)))

(defn part2 [dimensions]
  (apply + (map surface-area dimensions)))

(defn -main []
  (let [file "resources/input.txt"
        lines (with-open [reader (io/reader file)]
                (doall (map #(process-line %) (line-seq reader))))]
    (println (part1 lines))
    (println (part2 lines))))
