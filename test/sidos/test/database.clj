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
                        (s-property :nick-names :string :list)))

   (s-namespace :org.sidos.test.tasks
                (s-type :task
                        (s-property :description :string)
                        (s-property :assigned-to (>> :org.sidos.test.persons :person))))])

(def model (sidos.model/compile-model model-source))

(defn property-column-test []
  (println (sidos.database/property-column (-> model first :properties first))))

(defn create-tables-for-type-test []
  (sidos.database/with-connection db
    (sidos.database/drop-all)
    (sidos.database/create-tables-for-type (-> model first))
    (sidos.database/show-tables)))

(defn set-property-test []
  (sidos.database/with-connection db
    (sidos.database/drop-all)
    (sidos.database/create-tables-for-type (-> model first))
    (let [type-name "org.sidos.test.persons.person"
          id (sidos.database/create-instance type-name)]
      (sidos.database/set-property id type-name "name" "foo")
      (sidos.database/get-property id type-name "name"))))



(defn accessor-test []
  (sidos.database/with-connection db
    (sidos.database/drop-all)
    (sidos.database/create-tables-for-type (-> model first))
    (sidos.database/define-accessors model)
    (let [person (create-person)]
      (person-get-name person))))