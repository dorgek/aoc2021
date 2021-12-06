(ns aoc2021.day02
    (:use clojure.java.io))

(defn read_input [location]
    (def input_data [])
    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
            ( def input_data 
                (conj input_data 
                    (if (.contains line "forward") [(Integer. (re-find #"\d+" line)) 0] 
                        (if (.contains line "down") [0 (Integer. (re-find #"\d+" line))]  
                            [0 (- (Integer. (re-find #"\d+" line)))]
                        )
                    )
                )
            )
        )
    )

    (vec input_data)
)

(defn sum-tuples [[a b] [d e]]
    [(apply + [a d]) (apply + [b e])]
) 

(defn update-position [[a b c] [d e f]]
    [(apply + [a d]) (apply + [b (apply * [d c])]) (apply + [c f])]
)

(defn update-data [[a b]]
    [a 0 b]
)

(defn part1 [input_data]
    (def position (reduce sum-tuples input_data))

    (print "Part one: ")
    (println (apply * position))
)

(defn part2 [input_data] 
    (def depth-data (map update-data input_data))
    (def position (reduce update-position depth-data))

    (print "Part two: ")
    (println (apply * [(get position 0) (get position 1)]))
)

(defn day2 [] 
    (def input_data (read_input "resources/day02.txt"))
    
    (part1 input_data)
    (part2 input_data)
)