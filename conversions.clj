(ns conversions
  (:require [clojure.string :as str]
            [parsers :as parsers]))

;; --- Tablas basadas en tipo de ingrediente (type) ---
(def conversion-table
  {
   :oil         218
   :spice       288
   :vegetable   120
   :flour       150
   :baking_soda 250
   :cocoa       85
   :water-like  236
   :sugar       200
   :sugar_powder 120
   :butter      227
   :cheese      100
   :meat        220
   :fruit       130
   :cloves      5
   :grated      1
   :egg         1}) ; Unidad por huevo

(def calories-per-gram
  {
   :oil         9.0
   :spice       0.0
   :vegetable   0.6
   :flour       3.6
   :baking_soda 0.0
   :cocoa       4.0
   :water-like  0.0
   :sugar       4.0
   :sugar_powder 3.8
   :butter      7.2
   :cheese      4.0
   :meat        2.5
   :fruit       0.8
   :cloves      0.0
   :grated      0.0
   :egg         78.0}) ; Calorías por huevo promedio

;; --- Conversión de unidades ---
(defn singularize-unit [unit]
  (let [unit-lc (str/lower-case unit)]
    (cond
      (= unit-lc "ounces") "oz"
      (= unit-lc "pounds") "lb"
      (and (str/ends-with? unit-lc "s")
           (> (count unit-lc) 1))
      (subs unit-lc 0 (dec (count unit-lc)))
      :else unit-lc)))

(defn parse-amount [amount-str]
  (let [parts (str/split amount-str #" ")]
    (cond
      (= (count parts) 2)
      (+ (Double/parseDouble (first parts))
         (let [[n d] (str/split (second parts) #"/")]
           (/ (Double/parseDouble n) (Double/parseDouble d))))
      (str/includes? amount-str "/")
      (let [[n d] (str/split amount-str #"/")]
        (/ (Double/parseDouble n) (Double/parseDouble d)))
      :else
      (Double/parseDouble amount-str))))

;; --- Calorías estimadas ---
(defn estimate-calories [ingred]
  (let [qty (:quantity ingred)
        unit (name (:unit ingred))
        type (:type ingred)
        cal-per-gram (get calories-per-gram type 0)]
    (cond
      ;; Si es tipo huevo, se calcula por unidad
      (= type :egg) (* qty cal-per-gram)
      ;; Si está en gramos
      (= unit "g") (* qty cal-per-gram)
      ;; Para tazas u otras unidades, usar conversiones si hay
      :else (let [grams (get conversion-table type)]
              (if grams
                (* (* qty grams) cal-per-gram)
                0)))))

;; --- Temperatura ---
(defn f-to-c [f] (Math/round (double (* (- f 32) (/ 5.0 9)))))
(defn c-to-f [c] (Math/round (double (+ (* c (/ 9.0 5)) 32))))

(defn convert-temp-text [text target-unit]
  (let [pattern (get parsers/regex "temperature")]
    (str/replace text pattern
      (fn [[_ temp unit]]
        (let [t (Integer/parseInt temp)
              norm-unit (str/lower-case unit)]
          (cond
            (and (#{ "f" "f°" "fahrenheit" } norm-unit) (= target-unit "C"))
            (str (f-to-c t) "°C")
            (and (#{ "c" "c°" "celsius" } norm-unit) (= target-unit "F"))
            (str (c-to-f t) "°F")
            :else (str t "°" (str/capitalize norm-unit))))))))

;; --- Escalar cantidades ---
(defn scale-quantity [cantidad actuales nuevas]
  (* cantidad (/ nuevas actuales)))

;; --- Conversión estructurada ---
(defn convert-ingredient-struct
  [ingred {:keys [sistema porciones-actuales porciones-nuevas]}]
  (let [amount (if (string? (:quantity ingred))
                 (parse-amount (:quantity ingred))
                 (:quantity ingred))
        scaled (scale-quantity amount porciones-actuales porciones-nuevas)
        unit (name (:unit ingred))
        type (:type ingred)]
    (cond
      (= sistema "cup")
      (assoc ingred :quantity scaled)

      (= sistema "metric")
      (let [g (get conversion-table type)]
        (assoc ingred
               :quantity (if g (Math/round (* scaled g)) scaled)
               :unit (if g 'g (:unit ingred)))))))

;; --- Agregar calorías a metadata ---
(defn update-metadata-with-calories [recipe]
  (let [total-cal (->> (:ingredients recipe)
                       (map estimate-calories)
                       (reduce + 0))]
    (update recipe :metadata assoc :calories (Math/round total-cal))))

;; --- Procesamiento de instrucciones ---
(defn analyze-instruction [instr ingredients target-temp-unit]
  (let [text (:text instr)
        converted-text (convert-temp-text text target-temp-unit)
        ingredient-names (map #(str/lower-case (:text %)) ingredients)
        found-ingreds (filter #(str/includes? (str/lower-case converted-text) %) ingredient-names)]
    (assoc instr
           :text converted-text
           :contains-temp? (not= converted-text text)
           :contains-ingredient? (not (empty? found-ingreds)))))

(defn analyze-instructions [recipe]
  (let [analyzed (map #(analyze-instruction % (:ingredients recipe)
                                            (name (:temperature-unit recipe)))
                      (:instructions recipe))]
    (assoc recipe :instructions analyzed)))

;; --- Conversión de receta completa ---
(defn convert-recipe
  "Convierte una receta estructurada según el sistema de unidades,
   escalado de porciones y sistema de temperatura."
  [recipe {:keys [sistema porciones-actuales porciones-nuevas temperature-unit]}]
  (let [converted-ingredients
        (map #(convert-ingredient-struct %
                 {:sistema sistema
                  :porciones-actuales porciones-actuales
                  :porciones-nuevas porciones-nuevas})
             (:ingredients recipe))

        updated-recipe
        (assoc recipe
               :ingredients converted-ingredients
               :servings porciones-nuevas
               :temperature-unit temperature-unit)

        recipe-with-calories
        (update-metadata-with-calories updated-recipe)

        final-recipe
        (analyze-instructions recipe-with-calories)]

    final-recipe))

;; --- Ejemplo de uso ---
(comment
  ;; --- Receta de prueba ---
  (def recipe
    (->Recipe
      :side-dish
      4
      :cup
      :Celsius
      {:title "Chimichurri Sauce"
       :description "Pan-Seared Steak with garlic butter."
       :author "Natasha of NatashasKitchen.com"}
      {:author "Natasha of NatashasKitchen.com"
       :servings 4
       :prep-time "5 mins"
       :cook-time "15 mins"
       :total-time "20mins"
       :ingredients "Ingredients"
       :instructions "Instructions:"}
      [(->Ingredient 2.0 "tablespoon" "olive oil" :oil)
       (->Ingredient 1.0 "teaspoon" "salt" :spice)
       (->Ingredient 0.5 "cup" "extra-virgin olive oil" :oil)]
      [(->Instruction 1 "Season the steak with salt and pepper." false false)
       (->Instruction 2 "Heat the oil in a skillet over high heat." false false)
       (->Instruction 3 "Preheat the oven to 325°F." false false)
       (->Instruction 4 "Season with 1 1/2 tsp salt and 1 tsp black pepper" false false)]))

  ;; --- Prueba de conversión ---
  (def converted
    (convert-recipe recipe {:sistema "metric"
                            :porciones-actuales 4
                            :porciones-nuevas 8
                            :temperature-unit "C"}))

  ;; --- Verificación de resultados ---
  (println "Ingredientes convertidos:" (:ingredients converted))
  (println "Calorías estimadas:" (get-in converted [:metadata :calories]))
  (println "Instrucciones transformadas:")
  (doseq [i (:instructions converted)] (println (:text i)))
)


