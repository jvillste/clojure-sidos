(ns sidos.core
  (:use org.bituf.clj-dbcp)
  (:use [clojure.contrib.sql :as sql :only ()]))


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


(def namespaces
  [[:fi-sirunsivut-person
    [:person
     [:name :string]
     [:nick-names :string :list]]]
   [:fi-sirunsivut-project
    [:import [:fi-sirunsivut-person :persons]]
    [:task
     [:description :string]
     [:assigned-to [:persons :person]]]]])


(defmacro dsl-keyword [name & keys]
  `(defn ~name [& values#] (assoc (zipmap [~@keys] values# ) :definition-type ~(keyword name))))

(defn filter-by-definition-type [type definitions]
  (filter #(= (:definition-type %1) type) definitions))

(defn namespace
  ([name & definitions]  {:definition-type :namespace
                          :name name
                          :types (filter-by-definition-type :type definitions)
                          :imports (filter-by-definition-type :import definitions)}))

(defn type
  ([name & definitions]  {:definition-type :type
                          :name name
                          :properties (filter-by-definition-type :property definitions)}))


(dsl-keyword alias :namespace-name :short-name)
(dsl-keyword property :name :type :collection-type)
(dsl-keyword >> :namespace :type)


(def model
  [(namespace :fi-sirunsivut-person
              (type :person
                    (property :name :string)
                    (property :nick-names :string :list)))

   (namespace :fi-sirunsivut-project
              (type :task
                    (property :description :string)
                    (property :assigned-to (>> :fi-sirunsivut-person :person))))])

(def org-sidos-primitive
  (namespace :org-sidos-primitive
             (type :string)
             (type :integer)
             (type :time)
             (type :boolean)))

(defn compile [namespaces]
  (let [namespaces-to-types (model-to-namespace-type-map (conj namespaces org-sidos-primitive))]
    (for [namespace namespaces
          type (:types namespace)
          property (:properties type)]
      (let [context (create-context (select-keys namespaces-to-types [(:name namespace) :org-sidos-primitive]))]
        (vector (:name namespace) context)))))

(defn model-to-namespace-type-map [model] (apply hash-map (mapcat #(vector (:name %) (map :name (:types %))) model)))

(defn create-context [namespaces-to-types] (apply merge-with hash-set (map #(zipmap (% namespaces-to-types)
                                                                                    (repeat %))
                                                                           (keys namespaces-to-types))))






