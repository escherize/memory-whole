(ns memory-whole.core-test
  (:require [memory-whole.core :as mem]
            [clojure.test :as t :refer [deftest is]]
            [clojure.edn :as edn]
            [memory-whole.memory :as memory]
            [clojure.string :as str]
            [mount.core :as mount]
            [clojure.repl :as repl]))

(defn start-fixture [test-fn]
  (memory/clear-db! ::memory/yes)
  (test-fn))

(t/use-fixtures :once start-fixture)

;; for testing locally defined functions that
;; are not the sole thing being defined on their own line
(defn send-squid [] 1)
(defn feed-owl [food] [:eating food])

(deftest can-find-sources
  (is (= "(defn feed-owl [food] [:eating food])" (mem/find-source (symbol #'feed-owl))))
  (is (= "(defn send-squid [] 1)" (mem/find-source (symbol #'send-squid)))))

(deftest deftrace-works
  (defn send-squid [] 1)
  (defn feed-owl [food] [:eating food])
  (mem/trace-vars #'feed-owl)
  (is (= [:eating :x] (feed-owl :x)))
  (is (= [:eating :x] (:output (memory/one "feed-owl"))))

    ;; fixme
  #_#_#_#_#_#_#_(is (= [:x] (:arguments (memory/one "feed-owl"))))
  (is (= "feed-owl" (:name (memory/one "feed-owl"))))
  (is (str/ends-with? (:file (memory/one "feed-owl") "") "memory_whole/core_test.clj"))
  (is (=
         "(deftrace feed-owl\n  ([] 2)\n  ([x] 3)\n  ([x y] 4))"
         (:source (memory/one "feed-owl"))))
  (is (= 3 (:output (memory/one "feed-owl"))))
  (is (= -770262672 (:ast_hash (memory/one "feed-owl"))))
  (is (= "memory-whole.core-test/feed-owl" (:full_name (memory/one "feed-owl")))))

#_
(deftest tracing-works
  (defn f2 ([] 2) ([x] 3) ([x y] 4))

  (untrace-vars #'f2)

  (trace-vars #'f2)

  (is (= 3 (f2 :x)))

  (is (= 3 (:output (memory/one "f2"))) "can recover output")

  (is (= [:x] (:arguments (memory/one "f2"))))
  (is (= "f2" (:name (memory/one "f2"))))
  (is (str/ends-with? (:file (memory/one "f2") "") "memory_whole/core_test.clj"))
  (is (= "(defn f2 ([] 2) ([x] 3) ([x y] 4))" (:source (memory/one "f2"))))
  (is (= 3 (:output (memory/one "f2"))))
  (is (= -1608487388 (:ast_hash (memory/one "f2"))))
  (is (= "memory-whole.core-test/f2" (:full_name (memory/one "f2")))))
#_
(deftest tracing-works-with-keys
  (defn f3 [{:keys [a b c] :as x}]
    (* a b c (reduce + (vals x))))

  (untrace-vars #'f3)

  (trace-vars #'f3)

  (is (= 393/10 (f3 {:a 3 :b 10 :c 1/10})))

  (is (= 393/10 (:output (memory/one "f3"))) "can recover output")

  (is (= [{:a 3, :b 10, :c 1/10}] (:arguments (memory/one "f3"))))

  (is (= "(defn f3 [{:keys [a b c] :as x}]\n    (* a b c (reduce + (vals x))))" (:source (memory/one "f3")))))
