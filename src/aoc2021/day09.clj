(ns aoc2021.day09
    (:use clojure.java.io)
)

(defn parse-int [s]
    (Integer/parseInt s)
)

(defn split-string [s]
    (vec (map parse-int (re-seq #"\d" s)))
)

(defn read-input [location]
    (with-open [rdr (reader location)]
        (vec (map split-string (line-seq rdr)))
    )
)

(defn less-than
    "Check element wise to see if all variables is less than the
     specified size"
    [[x y] [sizey sizex]]
    (and (< x sizex) (< y sizey))
)

(defn neighbours
  ([sizeyx yx] (neighbours [[-1 0] [1 0] [0 -1] [0 1]] sizeyx yx))
  ([deltas sizeyx yx]
     (filter (fn [new-yx]
               (every? #(and (< -1 %) (less-than new-yx sizeyx)) new-yx))
             (map #(vec (map + yx %))
                deltas)))
)

(defn get-neighbours [matrix xy]
    (map #(get-in matrix %) (neighbours [(count (peek matrix)) (count matrix)] xy))
)

(defn get-neighbours-indexed [matrix xy]
    (vec (map #(vector (get-in matrix %) %) (neighbours [(count (peek matrix)) (count matrix)] xy)))
)

(defn is-lowpoint [matrix [xy value]]
    (every? #(< value %) (get-neighbours matrix xy))
)

(defn row-lowpoint [input-data row]
    (filter #(is-lowpoint input-data %) row)
)

(defn get-index [idx-y row]
    (map-indexed (fn [idx-x itm] [[idx-y idx-x] itm]) row)
)

(defn flatten-one-level [coll]  
  (mapcat #(if (sequential? %) % [%]) coll)
)

(defn find-basin [matrix xy basin-values]
    (def basin (get-neighbours-indexed matrix xy))
    (def basin (filter #(not= 9 (first %)) basin))
    (def basin (set (map (fn [[idx itm]] itm) basin)))
    (def basin (set (filter #(not (.contains basin-values %)) basin)))

    (def new-basin-vals (clojure.set/union basin-values basin))

    (doseq [new-basin-val basin]
        (def basin-values (find-basin matrix new-basin-val new-basin-vals))
    )

    (vec new-basin-vals)
)

(defn part1 [input-data]
    (def indexed-data (map-indexed get-index input-data))
    (def low-points (flatten (map #(map (fn [[xy v]] v) (row-lowpoint input-data %)) indexed-data)))

    (println "Part one: " (apply + [(count low-points) (apply + low-points)]))
)

(defn part2 [input-data]
    (def indexed-data (map-indexed get-index input-data))
    (def low-points (map #(map (fn [[xy v]] xy) (row-lowpoint input-data %)) indexed-data))
    (def low-points (remove #(= (count %) 0) low-points))
    (def low-points (flatten-one-level low-points))

    (def basins (map #(find-basin input-data % (set [])) low-points))
    (def basins (sort > (map count basins)))

    (println "Part two: " (apply * (take 3 basins)))
)

(defn day9 []
    (def input-data (read-input "resources/day09.txt"))
    (part1 input-data)
    (part2 input-data)
)