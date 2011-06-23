(ns sidos.test.model
  (:require sidos.model)
  (:use clojure.test))

(deftest compile-property-test
  (is (= (sidos.model/compile-property {:person :sirunsivut.persons
                                        :string :sidos.primitives}
                                       {:name :name
                                        :collection-type :single
                                        :range :string
                                        :definition-type :s-property})
         {:name :name
          :range {:name :string
                  :namespace :sidos.primitives}
          :collection-type :single})))