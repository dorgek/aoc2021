(ns aoc2021.day13
    (:use clojure.java.io)
    (:require [clojure.string :as str])
)

(defn parse-int [s]
    (Integer/parseInt s)
)

(defn map-coordinates [line]
    (map parse-int (str/split line #","))
)

(defn parse-fold-instructions [line]
    (if (.contains line "y=") [0 (parse-int (re-find #"\d+" line))] [(parse-int (re-find #"\d+" line)) 0])
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (def lines (line-seq rdr))
        (def fold-instructions (filter #(.contains % "fold") lines))
        (def fold-instructions (doall (map parse-fold-instructions fold-instructions)))
        (def coordinates (doall (map map-coordinates (filter #(not (or (.contains %1 "fold") (= "" %1))) lines)))) ;; x,y
    )

    [coordinates fold-instructions]
)

(defn update-coordinate [coordinate fold]
    (def updated-coordinate coordinate)
    (if (not= (peek fold) 0)
        ;; y fold
        [
            (def fold-line (peek fold))
            (def ycoord (last coordinate))
            (if (> ycoord fold-line) (def updated-coordinate [(first coordinate) (apply - [ycoord (apply * [(apply - [ycoord fold-line]) 2])])]))
        ]
        ;;x fold
        [
             (def fold-line (first fold))
         (def xcoord (first coordinate))
             (if (> xcoord fold-line) (def updated-coordinate [(apply - [xcoord (apply * [(apply - [xcoord fold-line]) 2])]) (last coordinate)]))
        ]
    )

    (vec updated-coordinate)
)

(defn print-coordinates [coordinates]
    (def xcoords (vec (map (fn [[x y]] x) coordinates)))
    (def ycoords (vec (map (fn [[x y]] y) coordinates)))

    (doseq [x (range (apply min ycoords) (apply + [1 (apply max ycoords)]))]
        (doseq [y (range (apply min xcoords) (apply + [1 (apply max xcoords)]))]
            (if (.contains coordinates [y x]) (print "■") (print " "))
        )

        (println)
    )
)

(defn part1 [coordinates fold-instructions]
    (def first-instruction (first fold-instructions))
    (def folded-coordinates (set (map #(update-coordinate % first-instruction) coordinates)))

    (println "Part one: " (count folded-coordinates))
)

(defn part2 [coordinates fold-instructions]
    (def folded-coordinates coordinates)
    (doseq [instruction fold-instructions]
        (def folded-coordinates (set (map #(update-coordinate % instruction) folded-coordinates)))
    )

    (println folded-coordinates)

    (print-coordinates folded-coordinates)
)

(defn day13 []
    (println "Parsing input:")
    (time (let [coordinates fold-instructions] (read-input "resources/day13.txt")))

    (time (part1 coordinates fold-instructions))
    (time (part2 coordinates fold-instructions))
)