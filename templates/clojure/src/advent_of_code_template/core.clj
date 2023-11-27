(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]
            [clojure.string :as clojure.string]))

(defn process-line [line]
  (let [numbers (map #(Integer/parseInt %) (clojure.string/split line #"x"))]
    numbers))

(defn print-numbers [numbers]
  (println "Numbers: " numbers))

(defn -main []
  (let [file "resources/input.txt"]
    (with-open [reader (io/reader file)]
      (doseq [line (line-seq reader)]
        (print-numbers (process-line line))))))
