(ns aoc2021.day04
    (:use clojure.java.io)
    (:require [clojure.string :as str])
)

(defn parse-int [s]
    (Integer/parseInt s)
)

(defn read-input [location]
    (def bingo-round [])
    (def bingo-boards [])

    (with-open [rdr (reader location)]
        (def line-sequences (line-seq rdr))
        (def first-line (first line-sequences))

        (def bingo-round (map parse-int (str/split first-line #",")))
        (def bingo-board [])

        (doseq [line (drop 2 line-sequences)]
            (if (str/blank? line) 
                ;; true statement
                [(def bingo-boards (conj bingo-boards bingo-board))
                    (def bingo-board [])]
                ;; false statement
                (def bingo-board (conj bingo-board (map parse-int (str/split (clojure.string/trim line) #"\s+"))))
            )
        )
    )

    (def bingo-boards (conj bingo-boards bingo-board))

    [bingo-boards bingo-round]
)

(defn check-row [row marked-numbers] 
    (def row-marked true)
    (doseq [row-num row]
        (if (not (.contains marked-numbers row-num)) (def row-marked false) )
    )

    (if (== (count marked-numbers) 0) (def row-marked false))

    (not (not row-marked))
)

(defn check-boards [bingo-boards marked-numbers]
    (def completed-board [])
    (doseq [[bingo-board] (map list bingo-boards)]

        (doall (for [row (map vec bingo-board) :while (== (count completed-board) 0)]
            (if (check-row row marked-numbers) (def completed-board bingo-board))
        ))

        ;; transpose the bingo-board and check the columns
        (def rotated-bingo-board (apply map vector bingo-board))

        (doall (for [row rotated-bingo-board :while (== (count completed-board) 0)]
            (if (check-row row marked-numbers) (def completed-board bingo-board))
        ))
    )

    (vec completed-board)
)

(defn calculate-score [board winning-number marked-numbers]
    ;; get all unmarked numbers and sum them together
    (def sum-unmarked 0)

    (doseq [board-number (flatten board)]
        [
            (if (not (.contains marked-numbers board-number))
                    (def sum-unmarked (apply + [sum-unmarked board-number])))
        ]
    )

    (* sum-unmarked winning-number)
)

(defn part1 [bingo-boards bingo-round]
    (def marked-numbers [])
    (def winning-number 0)


    ;; run the game
    (doall (for [bingo-number bingo-round :while (== (count (check-boards bingo-boards marked-numbers)) 0)]
        [
            (def marked-numbers (conj marked-numbers bingo-number))
            (def winning-number bingo-number)
        ]
    )) 

    (def winning-board (check-boards bingo-boards marked-numbers))

    (print "Part one: ")
    (println (calculate-score winning-board winning-number marked-numbers))
)

(defn set= [list1 list2]
    (= (set list1) (set list2))
)

(defn part2 [bingo-boards bingo-round]
    (def marked-numbers [])
    (def last-won-board [])
    (def in-game-boards bingo-boards)

    (doall (for [bingo-number bingo-round :while (not= (count in-game-boards) 0)]
        [
            (def marked-numbers (conj marked-numbers bingo-number))
            (def winning-number bingo-number)
            (def won-board (check-boards in-game-boards marked-numbers))

            (while (not= (count won-board) 0) 
                [
                    (def in-game-boards (remove #(set= (flatten won-board) (flatten %)) in-game-boards))
                    (def last-won-board won-board)

                    (def won-board (check-boards in-game-boards marked-numbers))
                ]
            )
        ]
    )) 

    (print "Part two: ")
    (println (calculate-score last-won-board winning-number marked-numbers))
)

(defn day4 [] 
    (let [bingo-boards bingo-round] (read-input "resources/day04.txt"))

    (part1 bingo-boards bingo-round)
    (part2 bingo-boards bingo-round)
)