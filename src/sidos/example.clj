(ns sidos.example)

(def model
  [(namespace :fi-sirunsivut-person
              (type :person
                    (property :name :string)
                    (property :nick-names :string :list)))

   (namespace :fi-sirunsivut-project
              (type :task
                    (property :description :string)
                    (property :assigned-to (>> :fi-sirunsivut-person :person))))])
