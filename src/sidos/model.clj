(ns sidos.model)

;;--------------- Modelling DSL

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
(dsl-keyword property :name :range :collection-type)
(dsl-keyword >> :namespace :type)


;;--------------- Primitive model

(def org-sidos-primitive
  (namespace :org-sidos-primitive
             (type :string)
             (type :integer)
             (type :time)
             (type :boolean)))


;;--------------- Model compiler

(defn model-to-namespace-type-map [model] (apply hash-map (mapcat #(vector (:name %) (map :name (:types %))) model)))

(defn create-context [namespaces-to-types] (apply merge-with hash-set (map #(zipmap (% namespaces-to-types)
                                                                                    (repeat %))
                                                                           (keys namespaces-to-types))))
(defn compile [namespaces]
  (let [namespaces-to-types (model-to-namespace-type-map (conj namespaces org-sidos-primitive))]
    (for [namespace namespaces]
      (let [context (create-context (select-keys namespaces-to-types
                                                 [(:name namespace) :org-sidos-primitive]))]
        (for [type (:types namespace)]
          (hash-map :name (:name type)
                    :namespace (:name namespace)
                    :properties (for [property (:properties type)]
                                  (hash-map :name (:name property)
                                            :range (let [range-specification (:range property)]
                                                     (if (:definition-type range-specification)
                                                       (hash-map :name (:type range-specification)
                                                                 :namespace (:namespace range-specification)) 
                                                       (hash-map :name range-specification
                                                                 :namespace (range-specification context))))))))))))


;;--------------- Misc

(defn print-type [type]
  (do (println (:name type))
      (doseq [property (:properties type)]
        (println (str "  " (:name property))))))
