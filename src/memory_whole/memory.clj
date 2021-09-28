(ns memory-whole.memory
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str])
  (:import (clojure.lang RT)))

(def *db-uri (atom {:connection-uri "jdbc:sqlite:.memory-whole.db"}))

;; (defn set-db! [uri] (reset! *db-uri uri))

(defn db []
  (assoc @*db-uri :connection (jdbc/get-connection @*db-uri)))

(defn init-db! []
  (jdbc/execute!
   (db)
   "create table if not exists calls
(id INTEGER PRIMARY KEY,
 name TEXT,
 full_name TEXT,
 start_time INTEGER,
 arguments TEXT,
 arg_lists TEXT,
 file TEXT,
 line INTEGER,
 column INTEGER,
 source TEXT,
 ast_hash BIGINT,
 end_time INTEGER,
 output TEXT,
 exception TEXT)"))

(init-db!)

(defn clear-db! [yes]
  (if (= ::yes yes)
    (do
      (jdbc/execute! (db) "delete from calls")
      (init-db!))
    (println (str "Call this with " ::yes " if you want to clear it."))))

(defn insert-start!
  [start-info]
  (init-db!)
  (-> (jdbc/insert! (db) :calls start-info) first vals first))

(defn insert-end! [id end-info]
  (jdbc/update! (db) :calls end-info ["id = ?" id]))

(defn insert-thrown! [id end-info]
  (jdbc/update! (db) :calls end-info ["id = ?" id]))

(defn ^:private update-maybe [m k f] (if (contains? m k) (update m k f) m))

(defn ^:private kebab-case-keys [m]
  (zipmap
   (mapv (fn [k] (keyword (str/replace (name k) #"_" "-"))) (keys m))
   (vals m)))

;; Reading back the history:
(defn format-on-read [row] (let [r (fnil read-string "nil")]
                             (-> row
                                 kebab-case-keys
                                 (update-maybe :arguments r)
                                 (update-maybe :arg-lists r)
                                 (update-maybe :output r))))

(defn read-fn-name [fn-name]
  (cond
    (symbol? fn-name) (str fn-name)
    (string? fn-name) fn-name
    (var? fn-name) (str (:name (meta fn-name)))))

(defn one [fn-name]
  (let [fn-name (read-fn-name fn-name)]
    (some->
     (jdbc/query (db) ["select * from calls
                    where name = ?
                    order by start_time desc
                    limit 1" fn-name])
     first
     format-on-read)))

(defn many [fn-name & [limit]]
  (let [fn-name (read-fn-name fn-name)
        limit (or limit 10)]
    (some->>
     (jdbc/query (db) ["select * from calls
                      where name = ?
                      order by start_time desc
                      limit ?" fn-name limit])
     (mapv format-on-read))))

(defn select
  "Takes a vector that gets passed to jdbc/query"
  [q-vector]
  (let [result (jdbc/query (db) q-vector)]
    (cond
      (nil? result) nil
      (coll? result) (mapv format-on-read result)
      :else (format-on-read result))))
