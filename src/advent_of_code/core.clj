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