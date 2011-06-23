(ns sidos.database
  (:use org.bituf.clj-dbcp)
  (:use [clojure.contrib.sql :as sql :only ()])
  (:require sidos.model)
  (:require clojure.string))

(defn sql-type [{:keys [name namespace]}]
  (if (= namespace :org.sidos.primitive)
    (cond (= name :string) "varchar(50)"
          (= name :integer) "int"
          (= name :time) "timestamp"
          (= name :boolean) "boolean"
          :default nil)
    "uuid"))

(defn column-name [property] (model-name-to-sql-name (name (:name property))))

(defn model-name-to-sql-name [type-name] (clojure.string/replace type-name
                                                                 #"(\.|-)"
                                                                 "_"))


(defn type-table-name [type] (model-name-to-sql-name (sidos.model/full-name type)))



(defn property-column [property]
  (str (column-name property) " " (sql-type (:range property))))

(defn single-valued-properties [type] (filter #(= (:collection-type %) :single)
                                               (:properties type)))

(defn type-table-definition [type]
  (str "create table " (type-table-name type)
       " ( id uuid, "
       (apply str (interpose ", " (map property-column
                                       (single-valued-properties type))))
       " ) "))

(defn list-table-definition [domain property]
  (str "create table list_" (type-table-name domain) "_" (model-name-to-sql-name (name (:name property)))
       " ( subject uuid, value " (sql-type (:range property)) ", index int )"))

(defn execute-updates [& statements]
  (doseq [statement statements]
    (println statement)
    (sql/do-commands statement)))

(defn execute-update [statement & parameters]
  (do
    (println (str "\"" statement "\" " [(into [] parameters)]))
    (sql/do-prepared statement (into [] parameters))))

(defn execute-query [query]
  (do
    (println query)
    (sql/with-query-results rows
      query
      (doall rows))))

(defn drop-all [] (execute-updates "drop all objects"))

(defn show-tables []
  (execute-query ["show tables"]))

(defn create-tables-for-type [type]
  (execute-updates (type-table-definition type)
                   (apply str (map (partial list-table-definition type)
                                   (filter #(= (:collection-type %) :list)
                                           (:properties type))))))

(defn create-tables-for-model [model]
  (dorun (map create-tables-for-type model)))



(defmacro with-connection [db & body]
  `(sql/with-connection ~db ~@body))

(defn exists [type-name id]
  (= 0 (:count (first (execute-query [(str "select count(*) as count from " (model-name-to-sql-name type-name) " where id = ?") id])))))

(defn set-property [ id type-name property value ]
  (if (exists type-name id)
    (execute-update (str "insert into " (model-name-to-sql-name type-name) " ( " property ", id) values (?,?)")
                    value id)
    (execute-update (str "update " (model-name-to-sql-name type-name) " set " property " = ? where id = ?")
                    value id)))

(defn get-property [ id type-name property-name ]
  ((keyword property-name) (first (execute-query [(str "select "
                                                       property-name
                                                       " from "
                                                       (model-name-to-sql-name type-name)
                                                       " where id = ?")
                                                  id]))))

(defn create-instance [type-name]
  (let [id (java.util.UUID/randomUUID)]
    (execute-update (str "insert into " (model-name-to-sql-name type-name) " (id) values (?)")
                    id)
    id))



(defn create-function-symbol [type] (symbol (str "create-" (name (:name type)))))

(defn define-creator [type]
  (let [full-name (sidos.model/full-name type)]
    (intern *ns*
            (create-function-symbol type)
            (fn [] (create-instance full-name)))))

(defn get-function-symbol [type property] (symbol (str "get-" (name (:name type)) "-" (name (:name property)))))

(defn define-getter [type property]
  (let [property-name (:name property)
        type-full-name (sidos.model/full-name type)]
    (intern *ns*
            (get-function-symbol type property)
            (fn [id] (get-property id type-full-name property-name)))))

(defn set-function-symbol [type property] (symbol (str "set-" (name (:name type)) "-" (name (:name property)))))

(defn define-setter [type property]
  (let [property-name (name (:name property))
        type-full-name (sidos.model/full-name type)]
    (intern *ns*
            (set-function-symbol type property)
            (fn [id value] (set-property id type-full-name property-name value)))))

(defn define-api [model]
  (doseq [type model]
    (define-creator type)
    (doseq [property (:properties type)]
      (define-getter type property)
      (define-setter type property))))
