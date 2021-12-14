(ns aoc2021.day12
    (:use clojure.java.io)
    (:use clojure.set)
    (:require [clojure.string :as str])
)

(defn read-input [location]
    (def neighbours-map {})
    (with-open [rdr (reader location)]
        (doseq [line (line-seq rdr)]
            (def nodes (str/split line #"-"))
            (def neighbours (get neighbours-map (first nodes)))
            (def neighbours-map (assoc neighbours-map (first nodes) (set (conj neighbours (last nodes)))))
        )
    )

    ;; map other half of neighbours
    (doseq [[k v] neighbours-map]
        (doseq [neighbour-node v]
            (def neighbours (get neighbours-map neighbour-node))
            (def neighbours-map (assoc neighbours-map neighbour-node (set (conj neighbours k))))
        )
    )

    (assoc neighbours-map "end" #{})
)

(defn all-uppercase? [s]
  (= s (str/upper-case s))
)

(defn can-visit [itm visited can-visit-small-again]
    (or (all-uppercase? itm) (or (not (.contains visited itm)) (and (not= "start" itm) (not= "end" itm) can-visit-small-again)))
)

(defn flatten-one-level [coll]
    (mapcat #(if (sequential? %) % [%]) coll)
)

(defn next-paths-available [nodes neighbour]
    [neighbour (conj nodes neighbour)]
)

;; function returns the rest of the possible paths
(defn build-possible-paths [neighbours nodes visited found-path can-visit-small-again]
    (def node (peek nodes))
    (def available-neighbours (get neighbours node))
    (def found-paths (if (= node "end") (conj found-path nodes) found-path))

    (def available-neighbours (filter #(can-visit % visited can-visit-small-again) available-neighbours))
    (def next-paths (map #(next-paths-available nodes %) available-neighbours))

    (for [[neighbour next-path] next-paths]
        (last [
            (def visit-small (if (and (.contains visited neighbour) (not (all-uppercase? neighbour))) false can-visit-small-again))
            (def possible-path (into [] (build-possible-paths neighbours next-path (union visited #{neighbour}) found-paths visit-small)))
            (union found-paths possible-path)
        ])
    )
)

(defn part1 [neighbours]
    (def possible-paths (flatten-one-level (build-possible-paths neighbours ["start"] (set ["start"]) [] false)))
    (def possible-paths (set (filter #(= (peek %) "end") possible-paths)))

    (println "Part one: " (count possible-paths))
)

(defn part2 [neighbours]
    (def possible-paths (flatten-one-level (build-possible-paths neighbours ["start"] (set ["start"]) [] true)))
    (def possible-paths (set (filter #(= (peek %) "end") possible-paths)))

    (println "Part two: " (count possible-paths))
)

(defn day12 []
    (println "Reading input")
    (time (def input-data (read-input "resources/day12.txt")))

    (time (part1 input-data))
    (time (part2 input-data))
)