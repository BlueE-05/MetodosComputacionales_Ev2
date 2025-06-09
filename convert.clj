(ns conversions
  (:require [clojure.string :as str]))

;; --- Tablas de conversión y alias ---
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

  ; Tabla de calorías por gramo para cada ingrediente
(def calories-per-gram
  {
   "granulated sugar"       4.0
   "powdered sugar"         3.8
   "flour"                  3.6
   "all-purpose flour"      3.6
   "almond flour"           6.0
   "cocoa powder"           4.0
   "canola oil"             9.0
   "olive oil"              9.0
   "extra-virgin olive oil" 9.0
   "vegetable oil"          9.0
   "butter"                 7.2
   "unsalted butter"        7.2
   "heavy cream"            3.5
   "water"                  0.0
   "lemon juice"            0.2
   "white wine vinegar"     0.0
   "vanilla"                2.1
   "dark chocolate chips"   5.4
   "sea salt"               0.0
   "kosher salt"            0.0
   "parmesan cheese"        4.0
   "romano cheese"          4.0
  })

; Tabla de gramos por unidad para cada unidad volumétrica/peso
(def unit-conversion
  {
   "tablespoon" 15
   "teaspoon"   5
   "oz"         28.35
   "lb"         453.6
   "pint"       473.176
  })

; Unidades que no se convierten
(def non-convertible-units
  ["dash" "pinch" "clove" "sprig" "zest" "eggs" "egg" "garlic"])

; Alias para normalizar nombres de ingredientes
(def ingredient-aliases
  {
   "salt"            "sea salt"
   "black pepper"    "pepper"
   "vanilla extract" "vanilla"
   "ev olive oil"    "extra-virgin olive oil"
  })

;; --- Utilidades de normalización y parsing ---
(defn normalize-ingredient-name [name]
  ;; Convierte el nombre a minúsculas y busca un alias si existe
  (let [lc-name (str/lower-case name)]
    (get ingredient-aliases lc-name lc-name)))

(defn singularize-unit [unit]
  ;; Convierte unidades plurales a singulares y normaliza unidades irregulares
  (let [unit-lc (str/lower-case unit)]
    (cond
      (= unit-lc "ounces") "oz" ; Caso especial
      (= unit-lc "pounds") "lb" ; Caso especial
      ;; Si termina en "s" y la versión singular está en la tabla, la convierte
      (and (str/ends-with? unit-lc "s")
           (contains? unit-conversion (subs unit-lc 0 (dec (count unit-lc)))))
      (subs unit-lc 0 (dec (count unit-lc)))
      :else unit-lc))) ; Si no, regresa la unidad tal cual

(defn parse-amount
  [amount-str]
  (let [parts (str/split amount-str #" ")]
    (cond
      ;; Caso mixto: "1 1/2"
      (= (count parts) 2)
      (+ (Double/parseDouble (first parts))
         (let [[n d] (str/split (second parts) #"/")]
           (/ (Double/parseDouble n) (Double/parseDouble d))))
      ;; Caso fracción: "1/2"
      (str/includes? amount-str "/")
      (let [[n d] (str/split amount-str #"/")]
        (/ (Double/parseDouble n) (Double/parseDouble d)))
      ;; Caso decimal o entero
      :else
      (Double/parseDouble amount-str))))

;; --- Funciones de conversión de unidades ---
(defn cup-to-gram [ingredient cups]
  ;; Convierte tazas de un ingrediente a gramos usando la tabla
  (let [normalized (normalize-ingredient-name ingredient)]
    (if-let [grams-per-cup (get conversion-table normalized)]
      (* cups grams-per-cup)
      nil))) ; Si no está en la tabla, regresa nil

(defn gram-to-cup [ingredient grams]
  ;; Convierte gramos de un ingrediente a tazas usando la tabla
  (let [normalized (normalize-ingredient-name ingredient)]
    (if-let [grams-per-cup (get conversion-table normalized)]
      (/ grams grams-per-cup)
      nil))) ; Si no está en la tabla, regresa nil

(defn unit-to-gram [unit amount]
  ;; Convierte una cantidad de unidad volumétrica/peso a gramos usando la tabla
  (let [normalized-unit (singularize-unit unit)]
    (if-let [g (get unit-conversion normalized-unit)]
      (* amount g)
      nil))) ; Si no está en la tabla, regresa nil

;; --- Calorías estimadas por ingrediente ---
(defn estimate-calories [ingred]
  ;; Estima las calorías de un ingrediente estructurado {:quantity ... :unit ... :description ...}
  (let [desc (normalize-ingredient-name (:description ingred))
        qty  (:quantity ingred)
        unit (name (:unit ingred))
        grams (if (= unit "g") qty (unit-to-gram unit qty))
        cal-per-gram (get calories-per-gram desc 0)]
    (if (and grams cal-per-gram)
      (* grams cal-per-gram)
      0)))

;; --- Conversión de temperatura ---
(defn f-to-c [f]
  ;; Convierte Fahrenheit a Celsius, redondeando al entero más cercano
  (Math/round (double (* (- f 32) (/ 5.0 9)))))

(defn c-to-f [c]
  ;; Convierte Celsius a Fahrenheit, redondeando al entero más cercano
  (Math/round (double (+ (* c (/ 9.0 5)) 32))))

(defn convert-temp-text [text target-unit]
  ;; Busca temperaturas en un texto y las convierte a la unidad deseada
  (let [pattern #"(?i)(\d+)\s*°?\s*(f|c|f°|c°|fahrenheit|celsius)"]
    (str/replace text pattern
      (fn [[_ temp unit]]
        (let [t (Integer/parseInt temp)
              norm-unit (str/lower-case unit)]
          (cond
            ;; Si es Fahrenheit y queremos Celsius
            (and (#{ "f" "f°" "fahrenheit" } norm-unit) (= target-unit "C"))
            (str (f-to-c t) "°C")
            ;; Si es Celsius y queremos Fahrenheit
            (and (#{ "c" "c°" "celsius" } norm-unit) (= target-unit "F"))
            (str (c-to-f t) "°F")
            ;; Si ya está en la unidad deseada, solo normaliza el formato
            :else (str t "°" (str/capitalize norm-unit))))))))

;; --- Escalado proporcional ---
(defn scale-quantity [cantidad porciones-actuales porciones-nuevas]
  ;; Escala una cantidad de acuerdo al cambio de porciones
  (* cantidad (/ porciones-nuevas porciones-actuales)))

;; --- Conversión de líneas y estructuras de ingredientes ---
(defn convert-ingredient-line
  [line options]
  (let [{:keys [sistema porciones-actuales porciones-nuevas]} options
        parts (str/split line #" ")
        ;; Detecta si la cantidad es mixta (ej. "1 1/2") y la une
        amount-str (if (and (> (count parts) 2) (re-matches #"\d+" (first parts)))
                   (str (first parts) " " (second parts))
                   (first parts))
        ;; Selecciona la unidad correctamente según si la cantidad es mixta o simple
        unit (if (and (> (count parts) 2) (re-matches #"\d+" (first parts)))
             (nth parts 2)
             (second parts))
        ;; Extrae el ingrediente correctamente según el caso
        ingredient (->> (drop (if (and (> (count parts) 2) (re-matches #"\d+" (first parts))) 3 2) parts)
                      (str/join " ") str/lower-case)
        amount (parse-amount amount-str) ; Convierte la cantidad a número
        scaled (scale-quantity amount porciones-actuales porciones-nuevas) ; Escala la cantidad
        normalized (normalize-ingredient-name ingredient)] ; Normaliza el nombre del ingrediente
    (cond
      ;; Si el sistema es "cup", solo escala y muestra la cantidad
      (= sistema "cup")
      {:original line
       :scaled scaled
       :unit unit
       :ingredient normalized
       :converted (str scaled " " unit " " normalized)}

      ;; Si el sistema es "metric", convierte a gramos si es posible
      (= sistema "metric")
      (let [g (if (= unit "cup")
                (cup-to-gram normalized scaled)
                (unit-to-gram unit scaled))]
        {:original line
         :scaled scaled
         :unit "g"
         :ingredient normalized
         :converted (if g
                      (str (Math/round g) " g " normalized)
                      (str scaled " " unit " " normalized))}))))

(defn convert-ingredient-struct
  ;; Convierte un mapa de ingrediente {:quantity ... :unit ... :description ...} a sistema cup o métrico, escalando la cantidad
  [ingred {:keys [sistema porciones-actuales porciones-nuevas]}]
  (let [amount (if (string? (:quantity ingred))
                 (parse-amount (:quantity ingred)) ; Convierte la cantidad si es string
                 (:quantity ingred))
        scaled (scale-quantity amount porciones-actuales porciones-nuevas) ; Escala la cantidad
        unit (name (:unit ingred)) ; Convierte la unidad a string
        normalized (normalize-ingredient-name (:description ingred))] ; Normaliza el nombre del ingrediente
    (cond
      ;; Si el sistema es "cup", solo escala y normaliza
      (= sistema "cup")
      (assoc ingred
             :quantity scaled
             :unit (:unit ingred)
             :description normalized)

      ;; Si el sistema es "metric", convierte a gramos si es posible
      (= sistema "metric")
      (let [grams (if (= unit "cup")
                    (cup-to-gram normalized scaled)
                    (unit-to-gram unit scaled))]
        (assoc ingred
               :quantity (if grams (Math/round grams) scaled)
               :unit (if grams 'g (:unit ingred))
               :description normalized)))))

;; --- Ejemplos de uso ---
(println
  (convert-ingredient-line
    "1 1/2 cup almond flour"
    {:sistema "metric" :porciones-actuales 4 :porciones-nuevas 8}))
;; Esperado: {:original "1 1/2 cup almond flour", :scaled 3.0, :unit "g", :ingredient "almond flour", :converted "288 g almond flour"}

(println
  (convert-temp-text "Bake at 350 F for 30 minutes." "C"))
;; Esperado: "Bake at 177°C for 30 minutes."

(println
  (convert-ingredient-struct
    {:quantity "2/3" :unit :cup :description "granulated sugar"}
    {:sistema "metric" :porciones-actuales 4 :porciones-nuevas 8}))
;; Esperado: {:quantity 267, :unit g, :description granulated sugar, ...}
