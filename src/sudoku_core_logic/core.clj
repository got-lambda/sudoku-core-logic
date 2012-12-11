(ns sudoku-core-logic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))


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

(defn sudoku-constraino [vars [constraint & constraints]]
  (if (seq vars)
    (all
     (if (zero? constraint)
       succeed
       (== (first vars) constraint))
     (sudoku-constraino (next vars) constraints))
    succeed))

(defn get-squares [rows]
  (for [i (range 3) j (range 3)]
    (for [ii (range 3) jj (range 3)]
      (get-in rows [(+ ii (* 3 i)) (+ jj (* 3 j))]))))

(defn solve-sudoku [constraints]
  (first
   (run 1 [q]
     (let [cells       (repeatedly 81 lvar)
           rows        (mapv vec (partition 9 cells))
           columns     (apply map vector rows)
           squares     (get-squares rows)
           constraints (flatten constraints)]
       (all
        (== q rows)
        (sudoku-constraino cells constraints)
        (everyg distinctfd squares)
        (everyg distinctfd rows)
        (everyg distinctfd columns)
        (everyg #(infd % (domain 1 2 3 4 5 6 7 8 9)) cells))))))
