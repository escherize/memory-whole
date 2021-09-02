(ns memory-whole.core-test
  (:require [memory-whole.core :as mem]
            [clojure.test :as t :refer [deftest is]]
            [clojure.edn :as edn]
            [memory-whole.memory :as memory]
            [clojure.string :as str]
            [mount.core :as mount]
            [clojure.repl :as repl])
  (:import [clojure.lang RT]))

(println (str/replace (pr-str (re-seq #"[^:]+" (System/getProperty "java.class.path"))) #"bryanmaass" "me" ))

(defn start-fixture [test-fn]
  (memory/clear-db! ::memory/yes)
  (test-fn))

(t/use-fixtures :once start-fixture)

(defn feed-owl [food] [:eating food])

#_(deftest can-find-sources

  (is (= {} (meta #'feed-owl)))

  #_(is (= "(defn feed-owl [food] [:eating food])" (mem/find-source 'feed-owl))))

(println "(re-seq #\"[^:]+\" (System/getProperty \"java.class.path\"))" (re-seq #"[^:]+" (System/getProperty "java.class.path")))
;; try:
(println "(resolve 'feed-owl) => " (resolve 'feed-owl))
;; try:
(println "(:file (meta (resolve 'feed-owl))) => " (:file (meta (resolve 'feed-owl))))
;; finally try:
(println "(.getResourceAsStream (RT/baseLoader) (:file (meta (resolve 'feed-owl)))) => " (.getResourceAsStream (RT/baseLoader) (:file (meta (resolve 'feed-owl)))))
;; Where x is the symbol you pass to source-fn
(println "(repl/source-fn 'feed-owl) => " (repl/source-fn 'feed-owl))

(println "(mem/find-source 'feed-owl) => " (mem/find-source 'feed-owl))

(deftest deftrace-works
  (mem/trace-vars #'feed-owl)
  (is (= [:eating :x] (feed-owl :x)))
  (let [result (memory/one "feed-owl")]
    (is (= [:eating :x] (:output result)))
    (is (= [:x] (:arguments result)))
    (is (= {} result))
    )

  ;; fixme
  #_#_#_#_#_#_
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
