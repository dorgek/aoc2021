(ns aoc2021.day3
    (:use clojure.java.io))

(def BIT_SIZE 12)
(def BINARY_NUM 2r111111111111)

(defn read_input [location]
    (def input_data [])
    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
        (def input_data (conj input_data (Integer/parseInt line 2))))
    )

    (vec input_data)
)

(defn get-binary-digit [digit n]
    (if (bit-test digit n) 1 0)
)

;; function to map to the most common digit for n in input_data
(defn map-common-digit [input_data size n]
    (def digits-n (map #(get-binary-digit % n) input_data))
    (def num-ones (reduce + digits-n))
    (def num-zeros (- size num-ones))
    (if (< num-ones num-zeros) 0 1)
)

(defn to-binary-string [a b]
    (str a b)
)

(defn get-common-bits [input_data]
    (def nums-iterate (reverse (take BIT_SIZE (range))))
    (def input-size (count input_data))

    (map #(map-common-digit input_data input-size %) nums-iterate)
)

(defn matching-bit [b n digit]
    (not= b (get-binary-digit digit n))
)

(defn reduce-nums [input_data n b]
    (def input-size (count input_data))

    (def common-bit (map-common-digit input_data input-size n))
    (def common-bit (if (== b 0) (if (== common-bit 1) 0 1) common-bit))

    (remove #(matching-bit common-bit n %) input_data)
)

(defn get-rating-values [input_data b]
    (def nums-iterate (reverse (take BIT_SIZE (range))))
    (def rating-byte input_data)
    
    (doall (for [n nums-iterate :while (> (count rating-byte) 1)]
        [(def rating-byte (reduce-nums rating-byte n b))]
    ))

    (first rating-byte)
)

(defn part1 [input_data] 
    (def common-digits (get-common-bits input_data))
    
    (def gamma-rate (Integer/parseInt (reduce to-binary-string common-digits) 2))
    (def epsilon-rate (bit-and-not BINARY_NUM gamma-rate))

    (print "Part one: ")
    (println (apply * [gamma-rate epsilon-rate]))
)

(defn part2 [input_data]
    (def oxygen-rating (get-rating-values input_data 1))
    (def co2-rating (get-rating-values input_data 0))

    (print "Part two: ")
    (println (apply * [oxygen-rating co2-rating]))
)

(defn day3 []
    (def input_data (read_input "resources/day3.txt"))

    (part1 input_data)
    (part2 input_data)
)