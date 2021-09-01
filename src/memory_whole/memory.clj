(ns memory-whole.memory
  (:require
   [mount.core :as mount]
   [clojure.java.jdbc :as jdbc]
   [clojure.string :as str]
   [memory-whole.memory :as mem]))

(def *db-uri (atom {:connection-uri "jdbc:sqlite:.memory-whole.db"}))

(defn set-db!
  "Call with something jdbc can use as a db spec before calling `mount/start`.
  For reference: https://github.com/clojure/java.jdbc#example-usage"
  [uri]
  (reset! *db-uri uri))

(defn on-start []
  (let [spec @*db-uri
        conn (jdbc/get-connection spec)]
    (assoc spec :connection conn)))

(mount/defstate ^{:on-reload :noop}
  db
  :start (on-start)
  :stop (do (-> db :connection .close) nil))

(defn init-db []
  (mount/start #'db)
  (jdbc/execute!
   db
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

(defn clear-db! [yes]
  (if (= ::yes yes)
    (do
      (mount/start #'db)
      (try
        (jdbc/execute! db "drop table calls")
        (catch Throwable _ (println "calls table is not there")))
      (init-db))
    (println (str "Call this with " ::yes " if you want to clear it."))))

(comment

  (clear-db! ::yes)

  )

(defn insert-start!
  [db start-info]
  (init-db)
  (->
   (jdbc/insert! db :calls start-info)
   first
   ((keyword "last_insert_rowid()"))))

(defn insert-end! [db id end-info]
  (jdbc/update! db :calls end-info ["id = ?" id]))

(defn insert-thrown! [db id end-info]
  (jdbc/update! db :calls end-info ["id = ?" id]))

(defn update-maybe [m k f]
  (if (contains? m k)
    (update m k f)
    m))

;; Reading back the history:
(defn format-on-read [row] (let [r (fnil read-string "nil")]
                             (-> row
                                 (update-maybe :arguments r)
                                 (update-maybe :arg_lists r)
                                 (update-maybe :output r))))

(defn read-fn-name [fn-name]
  (cond
    (symbol? fn-name) (str fn-name)
    (string? fn-name) fn-name
    (var? fn-name) (str (:name (meta fn-name)))))

(defn one [fn-name]
  (let [fn-name (read-fn-name fn-name)]
    (mount/start #'db)
    (some->
     (jdbc/query db ["select * from calls
                    where name = ?
                    order by start_time desc
                    limit 1" fn-name])
     first
     format-on-read)))

(defn many [fn-name & [limit]]
  (mount/start #'db)
  (let [fn-name (read-fn-name fn-name)
        limit (or limit 10)]
    (some->>
     (jdbc/query db ["select * from calls
                      where name = ?
                      order by start_time desc
                      limit ?" fn-name limit])
     (mapv format-on-read))))

(defn select
  "Takes a vector that gets passed to jdbc/query"
  [q-vector]
  (mount/start #'db)
  (let [result (jdbc/query db q-vector)]
    (cond
      (nil? result) nil
      (coll? result) (mapv format-on-read result)
      :else (format-on-read result))))
