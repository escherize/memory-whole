(ns advent-of-code.core
  (:require [memory-whole.core :as mem]
            ;[advent-of-code.scratch :as scratch]
            [clojure.java.io :as io]
            [clojure.string :as str])
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
;; => #'advent-of-code.core/sum-to-2020

(sum-to-2020 0 0)
;; => false

(mem/one "sum-to-2020")
{:arguments [0 0], :name "sum-to-2020", :file "/Users/bryanmaass/dv/memory-whole/example/advent_of_code/src/advent_of_code/core.clj", :start_time 1632788976376, :source "{:sym advent-of-code.core/sum-to-2020, :var #'advent-of-code.core/sum-to-2020, :meta {:arglists ([& args__1092__auto__]), :doc \"\", :line 25, :column 1, :file \"/Users/bryanmaass/dv/memory-whole/example/advent_of_code/src/advent_of_code/core.clj\", :name sum-to-2020, :ns #namespace[advent-of-code.core]}}", :output 0, :column 1, :end_time 1632788976379, :line 25, :id 1, :ast_hash -1908766230, :exception nil, :full_name "advent-of-code.core/sum-to-2020", :arg_lists nil}


(def query-results (mem/select ["select arguments, output from calls where name = 'sum-to-2020'"]))
;; => #'advent-of-code.core/query-results

query-results
;; => [{:arguments [0 0], :output 0}]

(reverse
 (sort-by first
          (mapv (juxt :output :arguments) query-results)))

;;=> ([0 [0 0]])

(mem/deftrace compute [input]
  (for [i input
        j input
        :when (and (>= j i) (sum-to-2020 i j))]
    [i j (* i j)]))

(mem/select ["select count(*) from calls;"])
;; => [{:count(*) 1}]
;;
(compute sample-input)
;; => [299 713 514579]

(mem/select ["select count(*) from calls;"])
;; => [{:count(*) 23}]

(mem/one "compute")
;; => {:arguments [[1721 979 366 299 675 1456]],
;;    :name "compute",
;;    :file "/Users/bryanmaass/dv/memory-whole/example/advent_of_code/src/advent_of_code/core.clj",
;;    :start_time 1632789009692,
;;    :source "{:sym advent-of-code.core/compute, :var #'advent-of-code.core/compute, :meta {:arglists ([& args__1092__auto__]), :doc \"\", :line 47, :column 1, :file \"/Users/bryanmaass/dv/memory-whole/example/advent_of_code/src/advent_of_code/core.clj\", :name compute, :ns #namespace[advent-of-code.core]}}",
;;    :output clojure.lang.LazySeq,
;;    :column 1,
;;    :end_time 1632789009696,
;;    :line 47,
;;    :id 2,
;;    :ast_hash -1971144331,
;;    :exception nil,
;;    :full_name "advent-of-code.core/compute",
;;    :arg_lists nil}

;; In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
;; them together produces 1721 * 299 = 514579, so the correct answer is 514579.

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello"))
