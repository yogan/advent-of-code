(ns advent-of-code-template.core
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))

(defn parse-draw [draw]
  (defn parse-color [draw color]
    (let [match (re-find (re-pattern (str "(\\d+) " color)) draw)]
      (if match (Integer/parseInt (second match)) 0)))
  (let [red   (parse-color draw "red")
        green (parse-color draw "green")
        blue  (parse-color draw "blue")]
    {:red red :green green :blue blue}))

(defn parse-line [line]
  (let [parts   (cstr/split line #":")
        game-id (Integer/parseInt (last (cstr/split (first parts) #" ")))
        draws   (map parse-draw (cstr/split (last parts) #";"))]
    [game-id draws]))

(defn is-draw-possible [draw]
  (and (<= (:red   draw) 12)
       (<= (:green draw) 13)
       (<= (:blue  draw) 14)))

(defn all-draws-possible [draws]
  (every? is-draw-possible draws))

(defn is-game-possible [game]
  (all-draws-possible (second game)))

(defn possible-game-ids [games]
  (let [possible-games (filter is-game-possible games)]
    (map first possible-games)))

(defn part1 [games]
  (apply + (possible-game-ids games)))

(defn part2 [games] "TODO")

(defn -main []
  (let [file "resources/input.txt"
  ; (let [file "resources/sample.txt"
        games (with-open [reader (io/reader file)]
                (doall (map #(parse-line %) (line-seq reader))))]
    ; (println "-----------------------------")
    ; (println "games:" games)
    (println "Part 1:" (part1 games))
    (println "Part 2:" (part2 games))))
