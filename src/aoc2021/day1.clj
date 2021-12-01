(ns aoc2021.day1
    (:use clojure.java.io))

(defn read_input [location]
    (def input_data [])
    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
        (def input_data (conj input_data (Integer/parseInt line))))
    )

    (vec input_data)
)

(defn compare-results [[a b]]
    (if (> (compare b a) 0 ) 1 0)
)

(defn average-values [[a b c]]
    (apply + [a b c])
)

(defn count_result [data]
    (def comp_array 
        (map compare-results
            (map vec (partition 2 1 data)
    )) )

    (apply + comp_array)
)

(defn day1 []
    (def input_data (read_input "resources/day1.txt"))

    (print "Part one: ")
    (println ( count_result input_data ))

    (def moving_average (map average-values  (partition 3 1 input_data)))

    (print "Part one: ")
    (println ( count_result moving_average ))
)