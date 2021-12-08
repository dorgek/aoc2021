(ns aoc2021.day08
    (:use clojure.java.io)
    (:use clojure.set)
    (:require [clojure.string :as str])
)

(defn split-vec [line]
    (vec (map #(str/split % #"\s+") line))
)

(defn read-input [location]
    ;; [[wires] [segments]]
    (with-open [rdr (reader location)]
        (def input-data (vec (map #(str/split % #" \| ") (line-seq rdr))))
        (vec (map split-vec input-data))
    )
)

(defn part1 [input-data]
    (def display-data (flatten (map #(last %) input-data)))
    (def display-data (map #(count %) display-data))
    (def display-data (frequencies display-data))

    ;; find 1, 4, 7, 8 counts
    (def sum-occurances (apply + [(get display-data 2) (get display-data 4) (get display-data 3) (get display-data 7)]))
    (println "Part one: " sum-occurances)
)

(defn str-to-char-set [string]
    (set (seq (first string)))
)

(defn determine-codes [line]
    ;; determine codes 
    (def codes-map {})

    ;; extract the known segments 
    (def codes-map (assoc codes-map 1 (str-to-char-set (filter #(= (count %) 2) line))))
    (def codes-map (assoc codes-map 4 (str-to-char-set (filter #(= (count %) 4) line))))
    (def codes-map (assoc codes-map 7 (str-to-char-set (filter #(= (count %) 3) line))))
    (def codes-map (assoc codes-map 8 (str-to-char-set (filter #(= (count %) 7) line))))

    ;; determine other sequences based off count and if values exist within known sequences
    (def five-codes (map #(str-to-char-set [%]) (filter #(= (count %) 5) line))) ;; 2, 3, or 5
    (def six-codes (map #(str-to-char-set [%]) (filter #(= (count %) 6) line))) ;; 0, 6, 9
    (def seven-code (get codes-map 7))
    (def four-code (get codes-map 4))

    ;; determine 3 code, this is 5 in length and subset of 7
    (def codes-map (assoc codes-map 3 (first (filter #(subset? seven-code %) five-codes)))) 

    ;; determine 9 code, this is 6 in length and subset of 4
    (def codes-map (assoc codes-map 9 (first (filter #(subset? four-code %) six-codes))))

    ;; determine 0 code, this is 6 in length and not a subset of 4, and a subset of 7
    (def codes-map (assoc codes-map 0 (first (filter #(and (not (subset? four-code %)) (subset? seven-code %)) six-codes)))) 

    ;; determine 6 code, this is 6 in length, and not a subst of 4, and not a subset of 7
    (def codes-map (assoc codes-map 6 (first (filter #(and (not (subset? four-code %)) (not (subset? seven-code %))) six-codes)))) 

    ;; determine 2 code, this is 5 in length, not a subset of 7, and not a subset of 9
    (def nine-code (get codes-map 9))
    (def codes-map (assoc codes-map 2 (first (filter #(and (not (subset? % nine-code)) (not (subset? seven-code %))) five-codes)))) 

    ;; determine 5 code, this is 5 in length, not a subset of 7, and a subset of 9
    (def codes-map (assoc codes-map 5 (first (filter #(and (not (subset? seven-code %)) (subset? % nine-code)) five-codes))))

    (map-invert codes-map)
)

(defn map-segment-display [output]
    (map #(str-to-char-set [%]) output)
)

(defn map-output-to-code [[mappings outputs]]
    (Integer/parseInt (str/join (map #(str (get mappings %)) outputs)))
)

(defn part2 [input-data]
    (def signal-data (map #(first %) input-data))

    (def mappings (map #(vector (determine-codes (first %)) (map-segment-display (last %))) input-data))

    ;; map the output result to the determined code
    (def outputs (map map-output-to-code mappings))
    (println "Part two" (apply + outputs))
)

(defn day8 []
    (def input-data (read-input "resources/day08.txt"))

    (part1 input-data)
    (part2 input-data)
)