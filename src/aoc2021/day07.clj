(ns aoc2021.day07
    (:use clojure.java.io)
)

(defn parse-int [s]
    (Integer/parseInt s)
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (vec (map parse-int (re-seq #"\d+" (first (line-seq rdr)))))
    )
)

(defn sum-fuel [crab-positions idx]
    (apply + (map #(Math/abs (- idx %)) crab-positions))
)

(defn sum-fuel-weighted [crab-positions idx]
    (apply + (map #(/ (* (Math/abs (- idx %)) (apply + [(Math/abs (- idx %)) 1]) ) 2 ) crab-positions))
)

(defn part1 [input-data]
    (def idx (/ (count input-data) 2))
    (def sorted-data (vec (sort input-data)))
    (def final-pos (get sorted-data idx))

    (println "Part one: " (sum-fuel sorted-data final-pos))
)

(defn part2 [input-data]
    (def den (count input-data))
    (def sum-data (apply + input-data))
    (def average (float (/ sum-data den)))
    (def final-fuel-upper (sum-fuel-weighted input-data (int (Math/ceil average))))
    (def final-fuel-lower (sum-fuel-weighted input-data (int average)))

    (def final-fuel (if (< final-fuel-upper final-fuel-lower) final-fuel-upper final-fuel-lower))

    (println "Part two: " final-fuel)
)

(defn day7 [] 
    (def input-data (read-input "resources/day07.txt"))

    (part1 input-data)
    (part2 input-data)
)