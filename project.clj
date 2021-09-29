(defproject
  net.clojars.escherize/memory-whole "0.1.5"
  :description "Tracing to remember function args and return values"
  :url "https://github.com/escherize/memory-whole"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]

                 ;; clojure spec
                 [spec-provider "0.4.14"]

                 ;; malli
                 [metosin/malli "0.6.1"]

                 ;; sqlite
                 [org.clojure/java.jdbc "0.7.11"]
                 [org.xerial/sqlite-jdbc "3.34.0"]]
  :source-paths ["src" "test"]
  :repl-options {:init-ns memory-whole.core})
