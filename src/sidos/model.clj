(ns sidos.model)

;;--------------- Modelling DSL

(defn filter-by-definition-type [type definitions]
  (filter #(= (:definition-type %) type) definitions))

(defmacro s-namespace
  [namespace-name & definitions] `{:definition-type :s-namespace
                                   :name ~(name namespace-name)
                                   :types (filter-by-definition-type :s-type [~@definitions] )
                                   :aliases (filter-by-definition-type :s-alias [~@definitions])})

(defmacro s-type
  [type-name & definitions] `{:definition-type :s-type
                              :name ~(name type-name)
                              :properties (filter-by-definition-type :s-property [~@definitions])})


(defmacro s-property
  ([property-name range] `{:name ~(name property-name)
                           :range ~(if (symbol? range)
                                     (name range)
                                     range)
                           :collection-type :single
                           :definition-type :s-property})

  ([property-name range collection-type] `{:name ~(name property-name)
                                           :range ~(if (symbol? range)
                                                     (name range)
                                                     range)
                                           :collection-type ~(keyword (name collection-type))
                                           :definition-type :s-property}))


(defmacro s-alias
  [namespace-name short-name] `{:namespace-name ~(name namespace-name)
                                :short-name ~(name short-name)
                                :definition-type :s-alias})

(defmacro >>
  [namespace-name type-name] `{:namespace ~(name namespace-name)
                               :type ~(name type-name)
                               :definition-type :>>})


;;--------------- Primitive model

(def org-sidos-primitive
  (s-namespace org.sidos.primitive
               (s-type string)
               (s-type integer)
               (s-type time)
               (s-type boolean)))


;;--------------- Model compiler

(defn model-to-namespace-type-map [model]
  (apply hash-map
         (mapcat #(vector (:name %)
                          (map :name
                               (:types %)))
                 model)))

(defn get-types-to-namespaces [namespaces-to-types]
  "namespaces-to-types = {''a'' (''b'') ''c'' (''d'' ''e'')}
   result: {''d'' ''c'', ''e'' ''c'', ''b'' ''a''}"
  (apply merge-with hash-set
         (map #(zipmap (namespaces-to-types %)
                       (repeat %))
              (keys namespaces-to-types) )))

(defn compile-property
  [context property]
  { :name (:name property)
   :range (let [range-specification (:range property)]
            (if (:definition-type range-specification)
              { :name (:type range-specification)
               :namespace (:namespace range-specification)}
              { :name range-specification
               :namespace (context range-specification)}))
   :collection-type (:collection-type property)})

(defn compile-type [context namespace type ] { :name (:name type)
                                              :namespace (:name namespace)
                                              :properties (map (partial compile-property context)  (:properties type))})

(defn compile-model [namespaces]
  (let [namespaces-to-types (model-to-namespace-type-map (conj namespaces org-sidos-primitive))]
    (apply concat (for [namespace namespaces]
                    (let [context (get-types-to-namespaces (select-keys namespaces-to-types
                                                                        [(:name namespace) "org.sidos.primitive"]))]

                      (map (partial compile-type
                                    context
                                    namespace)
                           (:types namespace) ))))))

;;--------------- Printing

(defn full-name [type-reference]
  (str (name (type-reference :namespace)) "." (name (type-reference :name))))

(defn print-type [type]
  (do (println (full-name type))
      (doseq [property (:properties type)]
        (println (str "  " (name (:name property))
                      " : " (full-name (:range property))
                      " " (name (:collection-type property)))))))

(defn print-model [model]
  (doseq [type model] (print-type type)))