(struct recipe
  (category            ; Symbol: e.g. 'dessert | 'all
   servings            ; Integer: e.g. 4
   unit_system         ; Symbol: e.g. 'cup, 'grams
   temperature_unit    ; Symbol: 'Celsius | 'Fahrenheit | 'NA
   analysis-time       ; Number: e.g. 5 (seconds)

   tokens              ; List of (cons Symbol String), e.g. '((title . "Delicious Pasta") (description . "bla bla bla") (author . "Chef John"))
   metadata            ; List of (cons Symbol String/Number), e.g. '((category . "dessert") (servings . 5) ...)
   ingredients         ; List of ingredient structs (defined below)
   instructions        ; List of instruction structs (defined below)
   )
  #:transparent)

(struct ingredient
  (quantity     ; String or Number: e.g. "1/2" or 200
   unit         ; Symbol: e.g. 'cup, 'grams, or #f
   description  ; String: e.g. "extra-virgin olive oil"
   type)        ; Symbol: e.g. 'oil, 'spice, etc.
  #:transparent)

(struct instruction
  (step         ; String: e.g. "1."
   text         ; String: description of the step
   temperature  ; Optional: temperature if appears, e.g. '(360 . 'Celsius)
   ingredient  ; Optional: ingredient if appears, e.g. above ingredient struct
   )
  #:transparent)

(define example-recipe
  (recipe
   'dessert
   4
   'cup
   'Celsius
   5
   '((title . "Delicious Pasta")
     (text . "A simple and delicious pasta recipe."))
     (author . "Chef John")
   '((category . "dessert")
     (servings . 4)
     (CaloriesPerServing . 200)
     (TotalCalories . 800)
     (PrepTime . "10 mins")
     (CookTime . "20 mins"))
   (list
    (ingredient "1/2" 'cup "extra-virgin olive oil" 'oil)
    (ingredient "1" #f "garlic clove, minced" 'vegetable)
    (ingredient 200 'grams "all-purpose flour" 'grain))
   (list
    (instruction "1." "Boil water in a large pot." #f)
    (instruction "2." "Add pasta and cook." '(360 . 'Celsius))
    (instruction "3." "Drain and serve with sauce." #f))))
