(ns memory-whole.core
  (:require [memory-whole.memory :as mem]
            [memory-whole.trace
             :as trace
             :refer [trace-vars
                     untrace-vars
                     deftrace]]))

(def one mem/one)
(def many mem/many)
