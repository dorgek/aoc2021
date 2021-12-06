(ns aoc2021.day05
    (:use clojure.java.io)
)

(defprotocol IPoint 
    (getX [this])
    (getY [this])    
)

(deftype Point [x y]
    IPoint ; implement IPoint protocol

    (getX [this] x)
    (getY [this] y)
)

(defn to-point[x y]
    (Point. x y)
)

(defprotocol ILine
    (getPoints [this diagonal])
    (getPoint1 [this])
    (getPoint2 [this])
)

(deftype Line [p1 p2]
    ILine ;; implement the ILine protocol

    (getPoints [this diagonal]
        (last [
            (def x (if (< (.getX p1) (.getX p2)) (range (.getX p1) (apply + [(.getX p2) 1])) (reverse (range (.getX p2) (apply + [(.getX p1) 1])))))
            (def y (if (< (.getY p1) (.getY p2)) (range (.getY p1) (apply + [(.getY p2) 1])) (reverse (range (.getY p2) (apply + [(.getY p1) 1])))))

            (if (== (count x) 1 ) 
                ;; true condition
                (map #(to-point (.getX p1) %) y)
                ;; false condition
                (if (== (count y) 1 )
                    ;; true condition
                    (map #(to-point % (.getY p1)) x)
                    ;; false condition
                    (if diagonal (map to-point x y) [])
                    
                )
            )
        ])
    )

    (getPoint1 [this] p1)
    (getPoint2 [this] p2)
)

(defn to-line [v]
    (Line. (to-point (get v 0) (get v 1)) (to-point (get v 2) (get v 3)))
)

(defn read-input [location]
    (def input-data [])
    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
            (def nums-in-line [])
            (doseq [num (re-seq #"\d+" line)]
                (def nums-in-line (conj nums-in-line (Integer/parseInt num)))
            )

            (def input-data (conj input-data (to-line nums-in-line)))
        )
    )

    (vec input-data)
)

(defn part1 [input-data]
    (def all-points (flatten (map #(.getPoints % false) input-data)))
    (def all-points-coords (map #(vector (.getX %) (.getY %)) all-points))
    (def overlapping-count (count (map val (remove (comp #{1} val) (frequencies all-points-coords)))))

    (print "Part one: ")
    (println overlapping-count)
)

(defn part2 [input-data]
    (def all-points (flatten (map #(.getPoints % true) input-data)))
    (def all-points-coords (map #(vector (.getX %) (.getY %)) all-points))
    (def overlapping-count (count (map val (remove (comp #{1} val) (frequencies all-points-coords)))))

    (print "Part two: ")
    (println overlapping-count)
)

(defn day5 []
    (def input-data (read-input "resources/day05.txt"))

    (part1 input-data)
    (part2 input-data)
)