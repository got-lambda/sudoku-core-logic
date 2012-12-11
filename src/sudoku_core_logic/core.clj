(ns sudoku-core-logic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

(defn get-squares [cells]
  (->> cells
       (partition 3)
       (partition 3)
       (apply interleave)
       (partition 3)
       (map flatten)))

(defn solve-sudoku [constraints]
  (let [sudoku-domain (domain 1 2 3 4 5 6 7 8 9)
        cells         (repeatedly 81 lvar)
        rows          (mapv vec (partition 9 cells))
        columns       (apply map vector rows)
        squares       (get-squares cells)
        constraints   (map #(if (zero? %) (lvar) %)
                           (flatten constraints))]
    (first
     (run 1 [q]
       (everyg #(infd % sudoku-domain) cells)
       (== cells constraints)
       (== q rows)
       (everyg distinctfd rows)
       (everyg distinctfd columns)
       (everyg distinctfd squares)))))

(def super-hard-sudoku
  [[8 0 0  0 0 0  0 0 0]
   [0 0 3  6 0 0  0 0 0]
   [0 7 0  0 9 0  2 0 0]

   [0 5 0  0 0 7  0 0 0]
   [0 0 0  0 4 5  7 0 0]
   [0 0 0  1 0 0  0 3 0]

   [0 0 1  0 0 0  0 6 8]
   [0 0 8  5 0 0  0 1 0]
   [0 9 0  0 0 0  4 0 0]])
