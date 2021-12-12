(ns aoc2021.day10
    (:use clojure.java.io)
    (:use clojure.set)
)

(def opening-brackets ["{" "(" "[" "<"])
(def map-closing-brackets (hash-map "}" "{" ")" "(" "]" "[" ">" "<"))
(def map-error-values (hash-map ")" 3 "]" 57 "}" 1197 ">" 25137))
(def map-completed-values (hash-map ")" 1 "]" 2 "}" 3 ">" 4))
(def map-opening-brackets (map-invert map-closing-brackets))


(defn split-string [s]
    (vec (re-seq #"[\[\({<\]\)}>]" s))
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (vec (map split-string (line-seq rdr)))
    )
)

(defn get-corrupted-bracket [line]
    (def bracket-stack ())
    (def corrupted-bracket nil)

    (doseq [bracket line :while (nil? corrupted-bracket)] 
        (if (.contains opening-brackets bracket) 
            ;; true statement
            (def bracket-stack (conj bracket-stack bracket))
            ;; false statement
            [
                (def opening-bracket (peek bracket-stack))
                (def bracket-stack (pop bracket-stack))
                (def corresponding-opening-bracket (get map-closing-brackets bracket))

                (if (not= corresponding-opening-bracket  opening-bracket) (def corrupted-bracket bracket))
            ]
        )
    )

    (vec corrupted-bracket)
)

(defn get-incomplete-brackets [line]
    (def bracket-stack ())

    (doseq [bracket line] 
        (if (.contains opening-brackets bracket) 
            ;; true statement
            (def bracket-stack (conj bracket-stack bracket))
            ;; false statement
            [
                (def opening-bracket (peek bracket-stack))
                (def bracket-stack (pop bracket-stack))
                (def corresponding-opening-bracket (get map-closing-brackets bracket))
            ]
        )
    )

    (first (list bracket-stack))
)

(defn get-complete-brackets [line]
    (map #(get map-opening-brackets %) line)
)

(defn calculate-completed-score [idx1 idx2]
    (apply + [(apply * [idx1 5]) idx2])
)

(defn get-completed-score [line]
    (reduce calculate-completed-score (map #(get map-completed-values %) line))
)

(defn part1 [input-data]
    (def corrupted-brackets (flatten (filter #(not= (count % ) 0) (map get-corrupted-bracket input-data))))
    (def corrupted-brackets (map #(get map-error-values (str %)) corrupted-brackets))

    (println "Part one: " (apply + corrupted-brackets))
)

(defn part2 [input-data]
    (def incomplete-lines (remove #(not= (count (get-corrupted-bracket %)) 0) input-data))
    (def incomplete-lines (map get-incomplete-brackets incomplete-lines))
    (def complete-brackets (map get-complete-brackets incomplete-lines))
    (def complete-brackets-score (vec (sort (map get-completed-score complete-brackets))))
    (def middle-index (int (/ (count complete-brackets-score) 2)))

    (println "Part two: " (get complete-brackets-score middle-index))
)

(defn day10 [] 
    (def input-data (read-input "resources/day10.txt"))

    (part1 input-data)
    (part2 input-data)
)