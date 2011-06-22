(ns sidos.test.model
  (:require sidos.model))

(defn compile-property-test []
  (sidos.model/compile-property {:person :sirunsivut.persons :string :sidos.primitives}
                                {:name :name, :collection-type :single, :range :string, :definition-type :s-property}))