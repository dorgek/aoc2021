(ns aoc2021.day14
    (:use clojure.java.io)
    (:require [clojure.string :as str])
)

(defn map-insertion [line]
    (def polymers (re-seq #"\w+" line))
    [(first polymers) (last polymers)]
)

(defn build-polymer-map [polymer]
    (map #(vector (str/join %) 1) (partition 2 1 polymer))
)

(defn flatten-one-level [coll]
    (mapcat #(if (sequential? %) % [%]) coll)
)

(defn sum-common-amounts [polymer-map key]
    (apply + (map (fn [[key-value itm]] itm) (filter (fn [[key-value itm]] (= key-value key)) polymer-map)))
)


(defn merge-common-keys [polymer-map]
    (def keys-set (into #{} (map (fn [[key itm]] key) polymer-map)))
    (into {}  (map #(vector %1 (sum-common-amounts polymer-map %1)) keys-set))
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (def lines (line-seq rdr))
        (def polymer (re-seq #"\w" (first lines)))
        (def insertion-rules (doall (into {}  (map map-insertion (drop 2 lines)))))
        (def polymer-map (doall (build-polymer-map polymer)))
        (def polymer-map (merge-common-keys polymer-map))
        (def start-character (first polymer))
        (def end-character (last polymer))
    )

    [insertion-rules polymer-map start-character end-character]
)

(defn get-new-key [insertion-rules polymer amount]
    (def polymer-individual (re-seq #"\w" polymer))
    (def insertion-polymer (get insertion-rules polymer))
    (def new-polymer-chain (vector (first polymer-individual) insertion-polymer (last polymer-individual)))
    (def new-polymer-chain (map str/join (partition 2 1 new-polymer-chain)))

    (into [] (map #(vector % amount) new-polymer-chain))
)

(defn steps-polymer [polymer-map insertion-rules idx]
    (def new-polymer-map polymer-map)

    (doseq [i (range idx)]
        (def new-polymer-map (merge-common-keys (flatten-one-level (map (fn [[key itm]] (get-new-key insertion-rules key itm)) new-polymer-map))))
    )

    (vec new-polymer-map)
)

(defn create-polymer-map [polymer amount]
    (def polymer-individual (re-seq #"\w" polymer))
    (into [] (map #(vector % amount) polymer-individual))
)

(defn count-polymer-amount [polymer-map]
    (merge-common-keys (flatten-one-level (map (fn [[key itm]] (create-polymer-map key itm)) polymer-map)))
)

(defn part1 [insertion-rules polymer-map start-character end-character]
    (def new-polymer-map (steps-polymer polymer-map insertion-rules 10))

    (def polymer-count (count-polymer-amount new-polymer-map))
    (def polymer-count (assoc polymer-count start-character (apply + [(get polymer-count start-character) 1])))
    (def polymer-count (assoc polymer-count end-character (apply + [(get polymer-count end-character) 1])))
    (def polymer-count (into {} (map (fn [[key itm]] [key (/ itm 2)]) polymer-count)))
    (def polymer-count (vec (map (fn [[key itm]] itm) polymer-count)))

    (println "Part one: " (apply - [(apply max polymer-count) (apply min polymer-count)]))
)

(defn part2 [insertion-rules polymer-map start-character end-character]
    (def new-polymer-map (steps-polymer polymer-map insertion-rules 40))

    (def polymer-count (count-polymer-amount new-polymer-map))
    (def polymer-count (assoc polymer-count start-character (apply + [(get polymer-count start-character) 1])))
    (def polymer-count (assoc polymer-count end-character (apply + [(get polymer-count end-character) 1])))
    (def polymer-count (into {} (map (fn [[key itm]] [key (/ itm 2)]) polymer-count)))
    (def polymer-count (vec (map (fn [[key itm]] itm) polymer-count)))

    (println "Part two: " (apply - [(apply max polymer-count) (apply min polymer-count)]))
)


(defn day14 []
    (println "Parsing input..")
    (time (let [insertion-rules polymer-map start-character end-character]  (read-input "resources/day14.txt")))

    (time (part1 insertion-rules polymer-map start-character end-character))
    (time (part2 insertion-rules polymer-map start-character end-character))
)