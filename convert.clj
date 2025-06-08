(ns conversions
  (:require [clojure.string :as str]))

(ns conversions
  (:require [clojure.string :as str]))

;; Tabla de conversión taza → gramos por ingrediente (valores aproximados)
(def conversion-table
  {
   ;; Azúcares y harinas
   "granulated sugar"       200
   "powdered sugar"         120
   "flour"                  150
   "all-purpose flour"      150
   "almond flour"           96
   "cocoa powder"           85

   ;; Grasas y líquidos
   "canola oil"             218
   "olive oil"              218
   "extra-virgin olive oil" 218
   "vegetable oil"          218
   "butter"                 227
   "unsalted butter"        227
   "heavy cream"            240
   "water"                  236
   "lemon juice"            240
   "white wine vinegar"     240
   "vanilla"                208
   "dark chocolate chips"   170

   ;; Sales
   "sea salt"               288
   "kosher salt"            288

   ;; Lácteos / Quesos
   "parmesan cheese"        100
   "romano cheese"          100
  })

;; Conversión directa de unidades volumétricas/masas
(def unit-conversion
  {
   "tablespoon" 15
   "teaspoon"   5
   "oz"         28.35
   "lb"         453.6
   "pint"       473.176
  })

;; Ingredientes que no se convierten, pero pueden escalarse
(def non-convertible-units
  ["dash" "pinch" "clove" "sprig" "zest" "eggs" "egg" "garlic"])

;; Alias de ingredientes para unificarlos
(def ingredient-aliases
  {
   "salt"            "sea salt"
   "black pepper"    "pepper"
   "vanilla extract" "vanilla"
   "ev olive oil"    "extra-virgin olive oil"
  })

;; Normaliza el nombre del ingrediente
(defn normalize-ingredient-name [name]
  (let [lc-name (str/lower-case name)]
    (get ingredient-aliases lc-name lc-name)))

;; Convierte tazas a gramos
(defn cup-to-gram [ingredient cups]
  (let [normalized (normalize-ingredient-name ingredient)]
    (if-let [grams-per-cup (get conversion-table normalized)]
      (* cups grams-per-cup)
      nil)))

;; Convierte gramos a tazas
(defn gram-to-cup [ingredient grams]
  (let [normalized (normalize-ingredient-name ingredient)]
    (if-let [grams-per-cup (get conversion-table normalized)]
      (/ grams grams-per-cup)
      nil)))

(defn singularize-unit [unit]
  (let [unit-lc (str/lower-case unit)]
    (cond
      (= unit-lc "ounces") "oz"
      (= unit-lc "pounds") "lb"
      (and (str/ends-with? unit-lc "s")
           (contains? unit-conversion (subs unit-lc 0 (dec (count unit-lc)))))
      (subs unit-lc 0 (dec (count unit-lc)))
      :else unit-lc)))

(defn unit-to-gram [unit amount]
  (let [normalized-unit (singularize-unit unit)]
    (if-let [g (get unit-conversion normalized-unit)]
      (* amount g)
      nil)))

;; Conversión de temperatura
(defn f-to-c [f] (Math/round (double (* (- f 32) (/ 5.0 9)))))
(defn c-to-f [c] (Math/round (double (+ (* c (/ 9.0 5)) 32))))

;; Escalado proporcional
(defn scale-quantity [cantidad porciones-actuales porciones-nuevas]
  (* cantidad (/ porciones-nuevas porciones-actuales)))


(println "Pruebas")

;; Lemon Cake-1.txt
(println (cup-to-gram "all-purpose flour" 1))         ; 150
(println (cup-to-gram "almond flour" 0.5))            ; 48
(println (unit-to-gram "teaspoon" 1.5))               ; 7.5 (baking powder)
(println (unit-to-gram "teaspoon" 0.5))               ; 2.5 (kosher salt)
(println (cup-to-gram "extra-virgin olive oil" 0.5))  ; 109
(println (cup-to-gram "granulated sugar" 0.5))        ; 100
(println (unit-to-gram "teaspoon" 0.5))               ; 2.5 (vanilla extract)
(println (cup-to-gram "lemon juice" 0.5))             ; 120
(println (cup-to-gram "powdered sugar" 1))            ; 120
(println (f-to-c 350))                                ; 177

;; Best Homemade Brownies-1.txt
(println (cup-to-gram "granulated sugar" 1.5))        ; 300
(println (cup-to-gram "all-purpose flour" 0.75))      ; 112.5
(println (cup-to-gram "cocoa powder" 0.66))           ; 56.1
(println (cup-to-gram "powdered sugar" 0.5))          ; 60
(println (cup-to-gram "dark chocolate chips" 0.5))    ; 85
(println (unit-to-gram "teaspoon" 0.75))              ; 3.75 (sea salt)
(println (cup-to-gram "canola oil" 0.5))              ; 109
(println (unit-to-gram "tablespoon" 2))               ; 30 (water)
(println (unit-to-gram "teaspoon" 0.5))               ; 2.5 (vanilla)
(println (f-to-c 325))                                ; 163

;; Chimichurri Sauce.txt
(println (cup-to-gram "extra-virgin olive oil" 0.5))  ; 109
(println (unit-to-gram "tablespoon" 2))               ; 30 (white wine vinegar)
(println (unit-to-gram "teaspoon" 0.5))               ; 2.5 (sea salt)
(println (unit-to-gram "teaspoon" 0.5))               ; 2.5 (dried oregano)
(println (unit-to-gram "teaspoon" 0.25))              ; 1.25 (red pepper flakes)
(println (unit-to-gram "teaspoon" 0.25))              ; 1.25 (smoked paprika)

;; Fettuccine Alfredo.txt
(println (unit-to-gram "oz" 24))                      ; 680.4 (dry fettuccine pasta)
(println (cup-to-gram "butter" 1))                    ; 227
(println (unit-to-gram "pint" 0.75))                  ; 354.882 (heavy cream, en ml)
(println (cup-to-gram "romano cheese" 0.75))          ; 75
(println (cup-to-gram "parmesan cheese" 0.5))         ; 50

;; Pan-Seared Steak with Garlic Butter.txt
(println (unit-to-gram "lb" 2))                       ; 907.2 (steaks)
(println (unit-to-gram "tablespoon" 0.5))             ; 7.5 (vegetable oil)
(println (unit-to-gram "teaspoon" 1.5))               ; 7.5 (sea salt)
(println (unit-to-gram "teaspoon" 1))                 ; 5 (black pepper)
(println (unit-to-gram "tablespoon" 2))               ; 30 (unsalted butter)
(println (f-to-c 350))                                ; 177

;; Pruebas de alias (ingredient-aliases)
(println (cup-to-gram "vanilla extract" 1))           ; 208
(println (cup-to-gram "ev olive oil" 1))              ; 218

;; Pruebas de conversión inversa
(println (gram-to-cup "granulated sugar" 400))        ; 2
(println (gram-to-cup "all-purpose flour" 75))        ; 0.5

;; Prueba de escala de cantidad
;; Escala la cantidad 2 (por ejemplo, 2 tazas) de una receta pensada para 4 porciones a una versión para 8 porciones.
(println (scale-quantity 2 4 8))                      ; 4.0