(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]))

(defn dragon [a]
  (concat a "0" (reverse (map #(if (= % \0) \1 \0) a))))

(defn fill [data len]
  (if (>= (count data) len)
    (subs data 0 len)
    (recur (apply str (dragon data)) len)))

(defn checksum [data]
  (if (odd? (count data))
    (apply str data)
    (recur (map #(if (apply = %) \1 \0) (partition 2 data)))))

(defn dragon-checksum [input len]
  (let [data (fill input len)]
    (checksum data)))

(defn -main []
  (let [file "resources/input.txt"
        input (with-open [reader (io/reader file)]
                (seq (first (line-seq reader))))]
    (println "Part 1:" (dragon-checksum input 272))
    (println "Part 2:" (dragon-checksum input 35651584))))
