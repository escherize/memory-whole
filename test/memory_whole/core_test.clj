(ns memory-whole.core-test
  (:require [memory-whole.core :as mem]
            [clojure.test :as t :refer [deftest is]]
            [memory-whole.memory :as memory]
            [clojure.string :as str]
            [clojure.repl :as repl])
  (:import [clojure.lang RT]))

;;(println (str/replace (pr-str (re-seq #"[^:]+" (System/getProperty "java.class.path"))) #"bryanmaass" "me" ))

(defn start-fixture [test-fn]
  (memory/clear-db! ::memory/yes)
  (test-fn))

(t/use-fixtures :once start-fixture)

(def notable [:arguments :name :full-name :output :exception])

(deftest deftrace-works
  (mem/deftrace feed-owl [food] [:eating food])

  (is (= [:eating :mouse] (feed-owl :mouse)))

  (let [one-result (memory/one "feed-owl")]
    (is (= [:eating :mouse] (:output one-result)))
    (is (= [:mouse] (:arguments one-result)))
    (is (=
         {:arguments [:mouse]
          :name "feed-owl"
          :output [:eating :mouse]
          :full-name "memory-whole.core-test/feed-owl"
          :exception nil}
         (select-keys one-result notable)))
    (is (= "(mem/deftrace feed-owl [food] [:eating food])"
           (:source one-result)))))

(deftest tracing-works
  (defn f2
    ([] 2)
    ([x] 3)
    ([x y] 4))

  (mem/trace-vars #'f2)

  (is (= 3 (f2 :x)))

  (let [one-result (memory/one "f2")]
    (is (= 3 (:output one-result)) "can recover output")
    (is (= [:x] (:arguments one-result)))
    (is (= "f2" (:name one-result)))
    (is (str/ends-with? (:file one-result "") "memory_whole/core_test.clj"))
    ;; wtf?
    (is (= "(defn f2 ([] 2) ([x] 3) ([x y] 4))" (:source one-result)))
    (is (= 3 (:output one-result)))
    (is (= "memory-whole.core-test/f2" (:full-name one-result)))))

(comment
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

  (println "----- - - --- --- - - - -- -")

  (println (pr-str (mem/deftrace boo [x] 2)))
  (println (pr-str (boo 13)))

  ;; observe
  (println (pr-str (:fn-body (meta #'boo))))

  ;; then
  (println (pr-str (one "boo")))
  (println (pr-str (mapv (juxt :arguments :output) (many "boo")))))
