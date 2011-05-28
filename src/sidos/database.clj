(ns sidos.database
  (:use org.bituf.clj-dbcp)
  (:use [clojure.contrib.sql :as sql :only ()])
  (:require sidos.model))

(def db-h2 (db-spec (h2-memory-datasource))) ; creates in-memory instance

(defn crud
  []
  (let [table :emp
        orig-record {:id 1 :name "Bashir" :age 40}
        updt-record {:id 1 :name "Shabir" :age 50}
        drop-table  #(sql/do-commands "DROP TABLE emp")
        retrieve-fn #(sql/with-query-results rows
                       ["SELECT * FROM emp WHERE id=?" 1]
                       (first rows))]
    (sql/with-connection db-h2
      ;; drop table if pre-exists
      ;;      (try (drop-table)  (catch Exception _)) ; ignore exception
      ;; create table
      ;;      (sql/do-commands  "CREATE TABLE emp (id INT, name VARCHAR(50), age INT)")
      ;; insert
      (sql/insert-values table (keys orig-record) (vals orig-record))
      ;; retrieve
      (println (retrieve-fn))
      ;; update
      (sql/update-values table ["id=?" 1] updt-record)
      ;; drop table
      ;;     (drop-table)
      )))

(defn type-table-definition [type]
  (str "create table " (sidos.model/full-name type)
       " ( "
       (apply str (interpose ", " (map property-column
                                             (filter #(= (:collection-type %) :single)
                                                     (:properties type)))))
       " ) "))

(defn property-column [property]
  (str (name (:name property)) " " (sql-type (:range property))))

(defn sql-type [{:keys [name namespace]}]
  (if (= namespace :org.sidos.primitive)
    (cond (= name :string) "varchar(50)"
          (= name :integer) "int"
          (= name :time) "timestamp"
          (= name :boolean) "boolean"
          :default nil)
    "uuid"))