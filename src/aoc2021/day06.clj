(ns aoc2021.day06
    (:use clojure.java.io)
)

(defn read-input [location]
    (def input-fish [0 0 0 0 0 0 0 0 0])

    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
            (doseq [num (re-seq #"\d+" line)]
                (def num-alive (Integer/parseInt num))
                (def input-fish (assoc input-fish num-alive (apply + [(get input-fish num-alive) 1]) ))
            )
        )
    )

    (vec input-fish)
)

(defn next-iteration [fish-state]
    (def next-state (assoc fish-state 7 (apply + [(get fish-state 7) (first fish-state)])))
    (vec (concat (rest next-state) [(first next-state)]))
)

(defn run-sim [input-data idx]
    (def next-state input-data)
    (doseq [i (range idx)]
        (def next-state (next-iteration next-state))
    )

    (vec next-state)
)

(defn part1 [input-data]
    (def next-state (run-sim input-data 80))
    (println "Part one: " (apply + next-state))
)

(defn part2 [input-data]
    (def next-state (run-sim input-data 256))
    (println "Part two: "  (apply + next-state))
)

(defn day6 []
    (def input-data (read-input "resources/day06.txt"))

    (part1 input-data)    
    (part2 input-data)
)