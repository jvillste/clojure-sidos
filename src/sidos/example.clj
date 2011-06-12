(ns sidos.example
  (:use [sidos.model :only [s-namespace s-type s-property >>]]))

(def model
  [(s-namespace fi.sirunsivut.person
              (s-type person
                    (s-property name string)
                    (s-property nick-names string list)))

   (s-namespace fi.sirunsivut.project
              (s-type task
                    (s-property description int)
                    (s-property assigned-to (>> fi.sirunsivut.person person))))])

