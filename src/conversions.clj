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
   :egg         1}) 

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
   :egg         78.0}) 

;; --- Conversión de unidades ---
(defn singularize-unit [unit]
  (let [unit-lc (str/lower-case unit)]
    (cond
      (= unit-lc "ounces") "oz"
      (= unit-lc "pounds") "lb"
      (= unit-lc "grams") "g"
      (= unit-lc "milliliters") "ml"
      (= unit-lc "liters") "l"
      (and (str/ends-with? unit-lc "s")
           (> (count unit-lc) 1))
      (subs unit-lc 0 (dec (count unit-lc)))
      :else unit-lc)))

(defn parse-amount [amount-str]
  (try
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
        (Double/parseDouble amount-str)))
    (catch Exception e
;      (println "Error al parsear cantidad:" amount-str)
      0)))

;; --- Calorías estimadas ---
(defn estimate-calories [ingred]
  (let [qty (:quantity ingred)
        unit (when (:unit ingred) (name (:unit ingred)))
        type (cond
               (keyword? (:type ingred)) (:type ingred)
               (string? (:type ingred)) (keyword (:type ingred))
               :else nil)
        cal-per-gram (when type (get calories-per-gram type))]
    (cond
      (or (nil? type) (nil? qty))
      (do
;        (println "Advertencia: ingrediente sin tipo o cantidad, ignorando:" ingred)
        0)
      (nil? cal-per-gram)
      (do
;        (println "Advertencia: tipo de ingrediente desconocido para calorías:" type)
        0)
      (= type :egg) (* qty cal-per-gram)
      (= unit "g") (* qty cal-per-gram)
      :else (let [grams (get conversion-table type nil)]
              (if grams
                (* (* qty grams) cal-per-gram)
                (do
;                  (println "Advertencia: tipo de ingrediente desconocido para conversión:" type)
                  0))))))

;; --- Temperatura ---
(defn f-to-c [f] (Math/round (double (* (- f 32) (/ 5.0 9)))))
(defn c-to-f [c] (Math/round (double (+ (* c (/ 9.0 5)) 32))))

(defn convert-temp-text [text target-unit]
  (let [pattern (get parsers/regex "temperature")]
    (if (or (nil? pattern) (nil? text))
      text
      (str/replace text pattern
        (fn [[_ temp unit]]
          (let [t (Integer/parseInt temp)
                norm-unit (str/lower-case unit)]
            (cond
              (and (#{ "f" "f°" "fahrenheit" } norm-unit) (= target-unit "C"))
              (str (f-to-c t) "°C")
              (and (#{ "c" "c°" "celsius" } norm-unit) (= target-unit "F"))
              (str (c-to-f t) "°F")
              :else (str t "°" (str/capitalize norm-unit)))))))))

;; --- Escalar cantidades ---
(defn scale-quantity [cantidad actuales nuevas]
  (if (and (number? cantidad) (number? actuales) (number? nuevas) (pos? actuales))
    (* cantidad (/ nuevas actuales))
    cantidad)) ; si hay nil o no es número, regresa la cantidad original

;; --- Conversión estructurada ---
(defn convert-ingredient-struct
  [ingred {:keys [sistema porciones-actuales porciones-nuevas]}]
  (let [amount (if (string? (:quantity ingred))
                 (parse-amount (:quantity ingred))
                 (:quantity ingred))
        scaled (scale-quantity amount porciones-actuales porciones-nuevas)
        unit (when (:unit ingred) (name (:unit ingred))) ; <-- cambio aquí
        type (:type ingred)]
    (cond
      (= sistema "cup")
      (assoc ingred :quantity scaled)

      (= sistema "metric")
      (let [g (get conversion-table type)]
        (assoc ingred
               :quantity (if g (Math/round (double (* scaled g))) scaled)
               :unit (if g 'g (:unit ingred)))))))

;; --- Agregar calorías a metadata ---
(defn update-metadata-with-calories [recipe]
  (let [ingredientes-validos (filter #(and (some? (:type %)) (some? (:quantity %))) (:ingredients recipe))
        total-cal (->> ingredientes-validos
                       (map estimate-calories)
                       (reduce + 0))]
    (update recipe :metadata assoc :calories (if (number? total-cal) (Math/round (double total-cal)) 0))))

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

;; --- Calorías por porción ---
(defn calories-per-serving [recipe]
  (let [total (get-in recipe [:metadata :calories] 0)
        servings (:servings recipe)]
    (if (pos? servings)
      (Math/round (/ total servings))
      0)))

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
