(ns aoc2021.day15
    (:use clojure.java.io)
    (:require [clojure.string :as str])
)

(defn parse-int [s]
    (Integer/parseInt s)
)

(defn split-line [line]
    (map parse-int (re-seq #"\d" line))
)

(defn less-than
    "Check element wise to see if all variables is less than the
     specified size"
    [[x y] [sizey sizex]]
    (and (< x sizex) (< y sizey))
)

(defn get-matrix-neighbours
    ([sizeyx yx] (get-matrix-neighbours [[-1 0] [1 0] [0 -1] [0 1]] sizeyx yx))
    ([deltas sizeyx yx]
     (filter (fn [new-yx]
                (every? #(and (< -1 %) (less-than new-yx sizeyx)) new-yx))
             (map #(vec (map + yx %))
                  deltas)))
)

(defn get-neighbours [matrix xy]
    (map #(get-in matrix %) (get-matrix-neighbours [(count (peek matrix)) (count matrix)] xy))
)

(defn get-index [idx-y row]
    (vec (map-indexed (fn [idx-x itm] [[idx-y idx-x] itm]) row))
)

(defn flatten-one-level [coll]
    (mapcat #(if (sequential? %) % [%]) coll)
)

(defn build-neighbours [coordinate-vector]
    (def indexed-matrix (flatten-one-level (map-indexed get-index coordinate-vector)))
    (into {}  (map (fn [[location itm]] (vector itm (get-neighbours coordinate-vector location))) indexed-matrix))
)

(defn build-new-label-line [line]
    (map (fn [[location itm]] [(str/join "," location) itm]) line)
)

(defn generate-unique-labels [matrix]
    (def indexed-matrix (map-indexed get-index matrix))
    (map build-new-label-line indexed-matrix)
)

(defn flatten-labels [line]
    (into [] (map (fn [[label itm]] label) line))
)

(defn get-weight [values label-risk-map]
    (into {}  (map #(vector %1 (get label-risk-map %1)) values))
)

(defn get-weights [label-risk-map neighbours]
    (into {} (map (fn [[key value]] [key (get-weight value label-risk-map)]) neighbours))
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (into [] (map split-line (line-seq rdr)))
    )
)

(defn build-graph [coordinate-vector]
    (def label-matrix (generate-unique-labels coordinate-vector))
    (def unique-label-grid (into [] (map flatten-labels label-matrix)))
    (def graph (build-neighbours unique-label-grid)) ;; TODO: simplify this
    (def label-risk-map (into {} (flatten-one-level label-matrix)))

    (get-weights label-risk-map graph)
)

(defn prepare-costs [start graph]
    "Prepare the initial costs of the graph, and initialise the
    starting node to a cost of 0. This is in the format of:
        { node [weight parent] }"
    (-> (zipmap (keys graph) (repeat [Integer/MAX_VALUE nil]))
         (assoc start [0 start]))
)

(defn get-next-node [costs unvisited]
    (->> costs
         (filter (comp unvisited first))
         (sort-by (comp first second))
         ffirst)
)

(defn unwind-path
    "Restore path from A to B based on costs data"
    [a b costs]
    (letfn [(f [a b costs]
                (when-not (= a b)
                    (cons b (f a (second (costs b)) costs))))]
        (cons a (reverse (f a b costs)))))

(defn all-shortest-paths
    "Get shortest paths for all nodes, along with their costs"
    [start costs]
    (let [paths (->> (keys costs)
                     (remove #{start})
                     (map (fn [n] [n unwind-path start n costs])))]
        (into (hash-map)
            (map (fn [[n p]]
                     [n [(first (costs n)) p]])
                 paths)))
)

(defn process-neighbour
    [parent
     parent-cost
     costs
     [neighbour [old-cost edge-cost]]]
    (let [new-cost (+ parent-cost edge-cost)]
        (if (< new-cost old-cost)
            (assoc costs
                neighbour
                [new-cost parent])
            costs))
)

(defn neighbours
    "Get given node's neighbours along with their own costs and costs of corresponding edges.
    Example output is: {1 [7 10] 2 [4 15]}
                        ^  ^  ^
                        |  |  |
     neighbour node label  |  |
          neighbour cost ---  |
               edge cost ------"
    [node graph costs]
    (->> (graph node)
         (map (fn [[neighbour edge-cost]]
                  [neighbour [(first (costs neighbour)) edge-cost]]))
         (into {})))

;; TODO: optimise this algorithms
;; This algorithm implementation comes from Rosetta Code
(defn dijkstra
    "explanation"
    ([start end graph]
        (loop [costs (prepare-costs start graph)
               unvisited (set (keys graph))]                ;; bind variables into here
            (let [current-node (get-next-node costs unvisited)
                  current-cost (first (costs current-node))]
                (cond (nil? current-node)
                      (all-shortest-paths start costs)

                      (= current-node end)
                      [current-cost (unwind-path start end costs)]

                      :else
                      (recur (reduce (partial process-neighbour
                                              current-node
                                              current-cost)
                                     costs
                                     (filter (comp unvisited first)
                                             (neighbours current-node graph costs)))
                             (disj unvisited current-node))))))
    ([start graph] (dijkstra start nil graph))
)

(defn part1 [graph]
    (def cost (first (dijkstra "0,0" (->> (keys graph)
                                          (sort)
                                          (last)) graph)))
    (println "Part one: " cost)
)

;; TODO: come back and implement this
(defn prep-data-part2 [coordinate-vector]

)

(defn day15 []
    (println "Parsing input...")
    (time (def graph  (->> "resources/day15.txt"
                           (read-input)
                           (build-graph))))

    (time (part1 graph))
)