(ns sidos.test.database
  (:use org.bituf.clj-dbcp)
  (:use sidos.database)
  (:use sidos.model)
  (:use clojure.test))

(def db (db-spec (h2-memory-datasource)))

(def model-source
  [(s-namespace :org.sidos.test.persons
                (s-type :person
                        (s-property :name :string)
                        (s-property :nick_names :string :list)))

   (s-namespace :org.sidos.test.tasks
                (s-type :task
                        (s-property :description :string)
                        (s-property :assigned-to (>> :org.sidos.test.persons :person))))])


(def model (sidos.model/compile-model model-source))

(defn property-column-test []
  (println (sidos.database/property-column (-> model first :properties first))))

(defn create-tables-for-type-test []
  (do
    (sidos.database/drop-all db)
    (sidos.database/create-tables-for-type db (-> model first))
    (println (sidos.database/show-tables db))))


(defn create-tables-for-type-test []
  (do
    (sidos.database/drop-all db)
    (sidos.database/create-tables-for-type db (-> model first))
    (println (sidos.database/set-property db 123  :org.sidos.test.tasks.task :description "foo"))))