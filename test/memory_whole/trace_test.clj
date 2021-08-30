(ns memory-whole.trace-test
  (:require [memory-whole.trace :refer :all]
            [clojure.test :as t :refer [deftest is]]
            [clojure.edn :as edn]
            [memory-whole.memory :as mem]
            [clojure.string :as str]))

;; for testing locally defined functions that
;; are not the sole thing being defined on their own line
(defn f0 []) (defn my-function [] 1)

(deftest can-find-sources
  (is (= "(defn f0 [])"
         (find-source 'f0)))

  (is (= "(defn my-function [] 1)"
         (find-source 'my-function))))

(deftest deftrace-works
  (deftrace f1
    ([] 2)
    ([x] 3)
    ([x y] 4))

  (is (= 3 (f1 :x)))
  (is (= 3 (:output (mem/one "f1"))))

  (is (= [:x] (:arguments (mem/one "f1"))))
  (is (= "f1" (:name (mem/one "f1"))))
  (is (str/ends-with? (:file (mem/one "f1")) "memory_whole/trace_test.clj"))
  (is (= "(deftrace f1\n    ([] 2)\n    ([x] 3)\n    ([x y] 4))" (:source (mem/one "f1"))))
  (is (= 3 (:output (mem/one "f1"))))
  (is (= -1652512550 (:ast_hash (mem/one "f1"))))
  (is (= "memory-whole.trace-test/f1" (:full_name (mem/one "f1")))))

(deftest tracing-works
  (defn f2 ([] 2) ([x] 3) ([x y] 4))

  (untrace-vars #'f2)

  (trace-vars #'f2)

  (is (= 3 (f2 :x)))

  (is (= 3 (:output (mem/one "f2"))) "can recover output")

  (is (= [:x] (:arguments (mem/one "f2"))))
  (is (= "f2" (:name (mem/one "f2"))))
  (is (str/ends-with? (:file (mem/one "f2")) "memory_whole/trace_test.clj"))
  (is (= "(defn f2 ([] 2) ([x] 3) ([x y] 4))" (:source (mem/one "f2"))))
  (is (= 3 (:output (mem/one "f2"))))
  (is (= -1608487388 (:ast_hash (mem/one "f2"))))
  (is (= "memory-whole.trace-test/f2" (:full_name (mem/one "f2")))))

(deftest tracing-works-with-keys
  (defn f3 [{:keys [a b c] :as x}]
    (* a b c (reduce + (vals x))))

  (untrace-vars #'f3)

  (trace-vars #'f3)

  (is (= 393/10 (f3 {:a 3 :b 10 :c 1/10})))

  (is (= 393/10 (:output (mem/one "f3"))) "can recover output")

  (is (= [{:a 3, :b 10, :c 1/10}] (:arguments (mem/one "f3"))))

  (is (= "(defn f3 [{:keys [a b c] :as x}]\n    (* a b c (reduce + (vals x))))" (:source (mem/one "f3")))))
