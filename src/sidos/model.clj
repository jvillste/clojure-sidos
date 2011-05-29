(ns sidos.model)

;;--------------- Modelling DSL

(defn filter-by-definition-type [type definitions]
  (filter #(= (:definition-type %1) type) definitions))

(defn s-namespace
  ([name & definitions]  {:definition-type :s-namespace
                          :name name
                          :types (filter-by-definition-type :s-type definitions)
                          :aliases (filter-by-definition-type :s-alias definitions)}))

(defn s-type
  ([name & definitions]  {:definition-type :s-type
                          :name name
                          :properties (filter-by-definition-type :s-property definitions)}))


(defn s-property [& values] (let [result (assoc (zipmap [:name :range :collection-type] values) :definition-type :s-property)]
                              (if (nil? (:collection-type result))
                                (assoc result :collection-type :single)
                                result)))

(defmacro dsl-keyword [name & keys]
  `(defn ~name [& values#] (assoc (zipmap [~@keys] values# ) :definition-type ~(keyword name))))

(dsl-keyword s-alias :namespace-name :short-name)
(dsl-keyword >> :namespace :type)


;;--------------- Primitive model

(def org-sidos-primitive
  (s-namespace :org.sidos.primitive
               (s-type :string)
               (s-type :integer)
               (s-type :time)
               (s-type :boolean)))


;;--------------- Model compiler

(defn model-to-namespace-type-map [model] (apply hash-map (mapcat #(vector (:name %) (map :name (:types %))) model)))

(defn create-context [namespaces-to-types] (apply merge-with hash-set (map #(zipmap (% namespaces-to-types)
                                                                                    (repeat %))
                                                                           (keys namespaces-to-types))))
(defn compile-model [namespaces]
  (let [namespaces-to-types (model-to-namespace-type-map (conj namespaces org-sidos-primitive))]
    (apply concat (for [namespace namespaces]
                    (let [context (create-context (select-keys namespaces-to-types
                                                               [(:name namespace) :org.sidos.primitive]))]
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
                                                                               :namespace (range-specification context))))
                                                          :collection-type (:collection-type property))))))))))


;;--------------- Printing

(defn full-name [type-reference]
  (str (name (:namespace type-reference)) "." (name (:name type-reference))))

(defn print-type [type]
  (do (println (full-name type))
      (doseq [property (:properties type)]
        (println (str "  " (name (:name property))
                      " : " (full-name (:range property))
                      " " (name (:collection-type property)))))))

(defn print-model [model]
  (doseq [type model] (print-type type)))