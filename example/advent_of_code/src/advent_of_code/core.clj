(ns advent-of-code.core
  (:require [memory-whole.core :as mem])
  (:gen-class))



;;--- Day 1: Report Repair ---

;; After saving Christmas five years in a row, you've decided to take a vacation
;; at a nice resort on a tropical island. Surely, Christmas will go on without
;; you.

;; Before you leave, the Elves in accounting just need you to fix your expense
;; report (your puzzle input); apparently, something isn't quite adding up.

;; Specifically, they need you to find the two entries that sum to 2020 and then
;; multiply those two numbers together.

;; For example, suppose your expense report contained the following:

(def sample-input [1721 979 366 299 675 1456])

(mem/deftrace sum-to-2020 [a b] (= 2020 (+ a b)))

(mem/deftrace compute [input]
  (first
   (for [i input
         j input
         :when (and (> j i) (sum-to-2020 i j))]
     (* i j))))

(compute sample-input)
;; => 514579

(mem/one "compute")

;; In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
;; them together produces 1721 * 299 = 514579, so the correct answer is 514579.

;; Of course, your expense report is much larger. Find the two entries that sum
;; to 2020; what do you get if you multiply them together?

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
