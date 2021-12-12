(ns aoc2021.day11 
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
  ([sizeyx yx] (neighbours [[-1 0] [1 0] [0 -1] [0 1] [-1 -1] [-1 1] [1 -1] [1 1]] sizeyx yx))
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

(defn increment-line [l]
    (vec (map #(apply + [% 1]) l))
)

(defn get-index [idx-y row]
    (vec (map-indexed (fn [idx-x itm] [[idx-y idx-x] itm]) row))
)

(defn update-index [matrix location]
    (def current-value (get (get matrix (first location)) (last location)))
    (assoc-in matrix location (apply + [current-value 1]))
)

(defn flatten-one-level [coll]  
  (mapcat #(if (sequential? %) % [%]) coll)
)

(defn complete-flash [matrix location]
    (def neighbours-list (neighbours [(count (first matrix)) (count matrix)] location))
    (def new-matrix matrix)

    (doseq [neighbour neighbours-list]
        (def new-matrix (update-index new-matrix neighbour))
    )

    (vec new-matrix)
)

(defn count-num-flashes [line]
    (count (filter #(> % 9) line))
)

(defn contains-value [value set]
    (some #(= (compare value %) 0) (vec set))
)

(defn step [matrix]
    ;; step up all values 
    (def new-matrix (vec (map increment-line matrix)))
    (def indexed-matrix (map-indexed get-index new-matrix))

    ;; get initial flashes
    (def flashed-octopi (filter #(> (last %) 9) (flatten-one-level indexed-matrix)))
    (def to-flash (into () (map (fn [[location itm]] location) flashed-octopi)))
    (def flashed-set (set to-flash)) ;; initialise initial list to flash -> so we don't repeat later on

    (while (not= (count to-flash) 0)
        (def next-flash (peek to-flash))
        (def to-flash (pop to-flash))

        ;; update flashes around the neighbours
        (def new-matrix (complete-flash new-matrix next-flash))

        ;; check if neighbours require to be flashed now 
        (def indexed-matrix (map-indexed get-index new-matrix))
        (def flashed-octopi (filter #(> (last %) 9) (flatten-one-level indexed-matrix)))
        (def flashed-octopi (into () (map (fn [[location itm]] location) flashed-octopi)))
        (def can-flash (filter #(not (contains-value % flashed-set)) flashed-octopi))
    
        ;; update set with those that are going to be flashed
        (def flashed-set (clojure.set/union flashed-set can-flash))

        ;; push possible flashes onto stack
        (if (not= (count can-flash) 0) (def to-flash (into () (concat to-flash can-flash))))
    )

    ;; count how many flashes there where
    (def num-flashes (apply + (map count-num-flashes new-matrix)))

    ;; reset octopi flashes back to 0
    (doseq [flashed flashed-set]
        (def new-matrix (assoc-in new-matrix flashed 0))
    )

    [new-matrix num-flashes]
)

(defn part1 [input-data]
    (def matrix input-data)
    (def num-flashed 0)

    (doseq [v (range 100)]
        (let [new-matrix num-flashes] (step matrix))
        (def num-flashed (apply + [num-flashed num-flashes]))
        (def matrix new-matrix)
    )

    (println "Part one: " num-flashed)
)

(defn part2 [input-data]
    (def matrix input-data)
    (def synchronised false)
    (def idx 0)
    
    (while (not synchronised)
        (let [new-matrix num-flashes] (step matrix))
        (def idx (apply + [idx 1]))
        (def matrix new-matrix)
        (def temp (flatten matrix))

        ;; check if values have synchronized
        (def synchronised (every? #(= % 0) (flatten matrix)))
    )

    (println "Part two: " idx)
)

(defn day11 []
    (def input-data (read-input "resources/day11.txt"))

    (part1 input-data)
    (part2 input-data)
)