(ns advent-of-code.core
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(def data-1
  (map #(Integer/parseInt %) (str/split-lines (data/input 2021 1))))

(defn compare-numbers
  [[previous acc] actual]
  (if (> actual previous)
    [actual (inc acc)]
    [actual acc]))

(defn solution-day1-1
  []
  (reduce compare-numbers [(apply + (take 3 data-1)) 0] (drop 3 data-1)))

(defn make-measurement
  [data]
  (apply + (take 3 data)))

(defn solution-day1-2
  ([]
   (let [previous (make-measurement data-1)
         actual (make-measurement (rest data-1))]
     (solution-day1-2 [actual
                       (if (> actual previous) 1 0)]
                      (rest data-1))))
  ([[previous acc] to-treat]
   (let [actual (make-measurement to-treat)
         _ (println [previous acc])]
     (if (empty? to-treat)
       acc
       (solution-day1-2 [actual
                         (if (> actual previous) (inc acc) acc)]
                        (rest to-treat))))))


(def data-2
  (let [lines (str/split-lines (data/input 2021 2))]
    (->> lines
         (map #(str/split % #" "))
         (map (fn [[command value]] [command (Integer/parseInt value)])))))

(defn day2-1-reduce-fn
  [[x y] [command value]]
  (case command
    "forward" [(+ x value) y]
    "up" [x (- y value)]
    "down" [x (+ y value)]))

(defn solution-day2-1
  []
  (->> (reduce day2-1-reduce-fn [0 0] data-2)
       (apply *)))

(defn day2-2-reduce-fn
  [[x y z] [command value]]
  (case command
    "forward" [(+ x value) (+ y (* z value)) z]
    "up" [x y (- z value)]
    "down" [x y (+ z value)]))

(defn solution-day2-2
  []
  (->> (reduce day2-2-reduce-fn [0 0 0] data-2)
       (take 2)
       (apply *)))


(def day3-test-input
  "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010")

(def data-3
  (str/split-lines
    ;day3-test-input
    (data/input 2021 3)
    ))

(defn max-min-vals-of-col
  [col]
  (->> col
       frequencies
       ((fn [x] [(apply max-key val x)
                 (apply min-key val x)]))
       (map first)))

(defn str-to-dec
  [binary-str]
  (apply + (map-indexed (fn [idx v]
                          (* (Integer/parseInt (str v))
                             (Math/pow 2 idx)))
                        binary-str)))

(defn solution-day3-1
  []
  (let [min-max-pairs (->> (apply map vector data-3)
                           (map max-min-vals-of-col))
        mins-bin (->> min-max-pairs
                      (map second)
                      (reverse))
        maxs-bin (->> min-max-pairs
                      (map first)
                      (reverse))
        min-dec (str-to-dec mins-bin)
        max-dec (str-to-dec maxs-bin)
        ]
    ;min-max-pairs
    (* min-dec max-dec)))

(defn most-common-val
  [col]
  (->> col
       frequencies
       (sort-by key)
       (reverse)
       (sort-by val >)
       ffirst))

(defn less-common-val
  [col]
  (->> col
       frequencies
       (sort-by key)
       (sort-by val)
       ffirst))

(defn rating
  [comp-fn acc rank]
  (let [rest-cols (map #(apply str (drop rank %)) acc)
        first-col (map first rest-cols)
        val-to-filter (comp-fn first-col)
        vals-to-keep (filter #(= (nth % rank) val-to-filter) acc)]
    (if (= (count vals-to-keep) 1)
      (first vals-to-keep)
      (rating comp-fn vals-to-keep (inc rank)))))

(defn solution-day3-2
  []
  (let [o2 (rating most-common-val data-3 0)
        co2 (rating less-common-val data-3 0)
        o2-dec (str-to-dec (reverse (map str o2)))
        co2-dec (str-to-dec (reverse (map str co2)))
        _ (println o2-dec co2-dec)]
    (* o2-dec co2-dec)))

(def data4-test
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11 0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19

  3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7")

(def data4
  (data/input 2021 4))

(defn parse-card
  [raw-card]
  (let [lines (map #(remove empty? (str/split % #" ")) raw-card)
        columns (apply map vector lines)]
    (->> (concat lines columns)
         (map #(zipmap % (repeat nil))))))

(defn card-has-full-line?
  [card]
  (->> card
       (map vals)
       (some #(= % [true true true true true]))))

(defn update-card
  [card number]
  (map #(if (contains? % number)
          (assoc % number true)
          %)
       card))

(defn update-cards
  [cards number]
  (map #(update-card % number) cards))

(defn parse-data
  [data]
  (let [raw-data (->> (str/split data #"\n\n")
                      (map #(str/split-lines %)))
        numbers (str/split (ffirst raw-data) #",")
        cards (map parse-card (rest raw-data))]
    {:numbers numbers
     :cards cards}))

(defn get-nil-val-line
  [line]
  (->> (filter #(nil? (second %)) line)
       (map first)))

(defn sum-nil-keys
  [card]
  (->> (take 5 card)
       (mapcat get-nil-val-line)
       (map #(Integer/parseInt %))
       (apply +)))

(defn play-game
  ([]
   (let [{:keys [numbers cards]} (parse-data data4-test)]
     (play-game cards numbers)))
  ([cards numbers]
   (let [current-number (first numbers)
         updated-cards (update-cards cards current-number)
         winner? (filter card-has-full-line? updated-cards)]
     (if (or (seq winner?) (empty? numbers))
       (* (sum-nil-keys (first winner?)) (Integer/parseInt current-number))
       (play-game updated-cards (rest numbers))))))

(defn play-game1
  ([]
   (let [{:keys [numbers cards]} (parse-data data4)]
     (play-game1 cards numbers nil nil)))
  ([cards numbers previous-winners previous-number]
   (let [current-number (first numbers)
         updated-cards (update-cards cards current-number)
         [winners number] (if-let [new-winner (doto (seq (filter card-has-full-line? updated-cards)) println)]
                            [new-winner current-number]
                            [previous-winners previous-number])]
     (if (seq winners)
       (if (= 1 (count numbers))
         (* (sum-nil-keys (first winners)) (Integer/parseInt number))
         (recur (remove (set winners) updated-cards) (rest numbers) winners number))
       (recur updated-cards (rest numbers) winners number)))))


(def data5-test
  "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")

(def data5
  (data/input 2021 5))

(defn parse-data-5
  [raw]
  (->> (str/split-lines raw)
       (map #(str/split % #" -> "))
       (map (fn [[a b]] [(vec (str/split a #","))
                         (vec (str/split b #","))]))
       (map (fn [[[x1 y1] [x2 y2]]] [[(Integer/parseInt x1)
                                      (Integer/parseInt y1)]
                                     [(Integer/parseInt x2)
                                      (Integer/parseInt y2)]]))))

(defn init-matrix
  [max]
  (-> (for [x (range 0 (inc max))
            y (range 0 (inc max))]
        [x y])
      (zipmap (repeat 0))
      (into (sorted-set))))

(defn update-matrix
  [matrix [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (reduce #(update %1 [x1 %2] inc) matrix (range (min y1 y2) (inc (max y1 y2))))
    (= y1 y2) (reduce #(update %1 [%2 y1] inc) matrix (range (min x1 x2) (inc (max x1 x2))))
    :else matrix))

(defn solution5-1
  []
  (let [data (parse-data-5 data5)
        max-val (apply max (flatten data))
        matrix (init-matrix max-val)]
    (->> (reduce update-matrix matrix data)
         (filter #(> (second %) 1))
         count)))

(defn draw-line
  [[[x1 y1] [x2 y2]]]
  (let [sign-x (if (< x1 x2) 1 -1)
        sign-y (if (< y1 y2) 1 -1)
        dx (range x1 (+ sign-x x2) sign-x)
        dy (range y1 (+ sign-y y2) sign-y)]
    (map vector dx dy)))



(defn update-matrix-2
  [matrix [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (reduce #(update %1 [x1 %2] inc) matrix (range (min y1 y2) (inc (max y1 y2))))
    (= y1 y2) (reduce #(update %1 [%2 y1] inc) matrix (range (min x1 x2) (inc (max x1 x2))))
    :else (let [updated-pts (draw-line [[x1 y1] [x2 y2]])]
            (reduce #(update %1 %2 inc) matrix updated-pts))))


(defn solution5-2
  []
  (let [data (parse-data-5 data5)
        max-val (apply max (flatten data))
        matrix (init-matrix max-val)]
    (->> (reduce update-matrix-2 matrix data)
         (filter #(> (second %) 1))
         count)))


(def data6-test
  "3,4,3,1,2")

(defn parse-data6
  [raw]
  (map #(Integer/parseInt %) (str/split (str/trim raw) #",")))

(def data6 (data/input 2021 6))

(defn timer
  [lanternfish]
  (case lanternfish
    0 6
    (dec lanternfish)))

(defn children
  [lanternfishes]
  (let [new-children (repeat (count (filter #{0} lanternfishes)) 8)]
    new-children))

(defn solution-day6-1
  ([]
   (solution-day6-1 (parse-data6 data6) 80))
  ([lanternfishes day]
   (let [new-children (children lanternfishes)
         lanternfishes-aging (map timer lanternfishes)
         lanternfishes-with-children (concat lanternfishes-aging new-children)
         ;_ (println "day" (- 13 day) ": " lanternfishes-with-children)
         ]
     (if (zero? day)
       (count lanternfishes)
       (recur lanternfishes-with-children (dec day))))))


(defn shift-ages
  [lanternfishes]
  (reduce-kv
    (fn [acc k v]
      (case k
        0 (assoc (update acc 6 (fnil + 0) v) 8 v)
        7 (update acc 6 (fnil + 0) v)
        (assoc acc (- k 1) v)))
    {}
    lanternfishes))

(defn parse-data6-2
  [raw]
  (frequencies (map #(Integer/parseInt %) (str/split (str/trim raw) #","))))

(defn solution-day6-2
  ([]
   (solution-day6-2 (parse-data6-2 data6) 256))
  ([lanternfishes day]
   (let [lanternfishes-aging (shift-ages lanternfishes)]
     (if (zero? day)
       (apply + (vals lanternfishes))
       (recur lanternfishes-aging (dec day))))))

(def data7
  (data/input 2021 7))

(def data7-test "16,1,2,0,4,2,7,1,2,14")
(def data7-bis "0,1,2000")

(defn parse-input7
  [raw]
  (map #(Integer/parseInt %) (str/split (str/trim raw) #",")))

(defn fuel-consumption
  [point crabs]
  (apply + (map #(Math/abs (- point %)) crabs)))

(defn solution7-1
  []
  (let [crabs (parse-input7 data7-bis)
        max-val (apply max crabs)
        min-val (apply min crabs)
        segment (range min-val max-val)]
    (->> (reduce #(assoc %1 %2 (fuel-consumption %2 crabs)) {} segment)
         (apply min-key val))))

(defn consumption
  [distance]
  (apply + (range 1 (inc distance))))

(defn fuel-consumption-2
  [point crabs]
  (apply + (map #(consumption (Math/abs (- point %))) crabs)))

(defn solution7-2
  []
  (let [crabs (parse-input7 data7)
        max-val (apply max crabs)
        min-val (apply min crabs)
        segment (range min-val max-val)]
    (->> (reduce #(assoc %1 %2 (fuel-consumption-2 %2 crabs)) {} segment)
         (apply min-key val))))


(def data8-test
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
   edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
   fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
   fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
   aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
   fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
   dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
   bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
   egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
   gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def data8
  (data/input 2021 8))

(defn parse-data8
  [raw]
  (let [p-fn #(str/split (str/trim %) #" ")
        ] (->> (str/split raw #"\n")
               (map #(str/split % #"\|"))
               (map (fn [[a b]] [(p-fn a) (p-fn b)])))))

(defn solution8-1
  []
  (let [input (parse-data8 data8)]
    (->> (map second input)
         (mapcat #(map count %))
         (filter #(some #{2 3 4 7} [%]))
         (count))))

(def real {1 #{"c" "f"}
           7 #{"a" "c" "f"}
           4 #{"b" "c" "d" "f"}
           3 #{"a" "c" "d" "f" "g"}
           2 #{"a" "c" "d" "e" "g"}
           5 #{"a" "b" "d" "f" "g"}
           0 #{"a" "b" "c" "e" "f" "g"}
           9 #{"a" "b" "c" "d" "f" "g"}
           6 #{"a" "b" "d" "e" "f" "g"}
           8 #{"a" "b" "c" "d" "e" "f" "g"}})

;(def mapping {"a"
;              "b"
;              "c"
;              "d"
;              "e"
;              "f"
;              "g"})

(defn find-a
  [patterns]
  (let [group-5 (patterns 5)
        group-6 (patterns 6)
        one (first (patterns 2))
        four (first (patterns 4))
        seven (first (patterns 3))
        eight (first (patterns 7))
        a (remove one seven)
        e (->> (map #(remove four %) group-5)
               flatten
               frequencies
               (apply min-key val)
               first)]
    (assoc patterns "a" (first a) "e" e)))


(defn manage-display
  [display]
  (let [patterns (group-by count (map set (first display)))]
    (->> patterns
         (find-a))))

;1 - 7 => a
;9 - 8 => e
;e => f via 5 et 6
;f => b
; e => d

;2 - 3 => ef
;2 - 5 => cfg
;3 - 5 =>

(def data9-test "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")

(def data9 (data/input 2021 9))

(defn parse-data9
  [raw]
  (->> (str/split-lines raw)
       (map #(str/split % #""))
       (map (fn [x] (into [] (map #(Integer/parseInt %) x))))
       (into [])))

(defn neighbours
  [eight width point]
  (let [deltas [[-1 0] [1 0] [0 -1] [0 1]]
        new-xy (map #(vec (map + point %)) deltas)]
    (filter (fn [[new-x new-y]]
              (and (< -1 new-x eight)
                   (< -1 new-y width))) new-xy)))

(defn smallest-in-neighbourhood?
  [matrix point eight width]
  (let [neighbours (neighbours eight width point)
        neighbours-vals (map #(get-in matrix %) neighbours)
        point-val (get-in matrix point)]
    (every? #(< point-val %) neighbours-vals)))

(defn matrix-coordinates
  [eight width]
  (for [x (range 0 eight)
        y (range 0 width)]
    [x y]))

(defn solution-day9-1
  []
  (let [matrix (parse-data9 data9)
        eight (count matrix)
        width (count (first matrix))
        matrix-coord (matrix-coordinates eight width)]
    (->> (filter #(smallest-in-neighbourhood? matrix % eight width) matrix-coord)
         (map #(get-in matrix %))
         (map inc)
         (apply +))))



(defn parse-data9-2
  [raw]
  (->> (into {} (map-indexed (fn [x line]
                               (into {}
                                     (map-indexed (fn [y v] [[x y] v]) line)))
                             (parse-data9 raw)))
       (remove #(= 9 (second %)))
       (map first)))



(defn adjacency
  [adjacent]
  (reduce (fn [acc [x y]] (let [neighbours (filter #(and (<= 0 (Math/abs (- x (first %))) 1)
                                                         (<= 0 (Math/abs (- y (second %))) 1)
                                                         (or (= x (first %)) (= y (second %)))
                                                         (not= [x y] %)) adjacent)]
                            (assoc acc [x y] neighbours))) {} adjacent))

(defn neighbours9
  [n graph visited]
  (let [to-be-visited (remove (set visited) (get graph n))]
    (if (seq to-be-visited)
      (recur (first to-be-visited) graph (concat visited to-be-visited))
      visited)))

(def memoize-neighbours9
  (memoize neighbours9))

