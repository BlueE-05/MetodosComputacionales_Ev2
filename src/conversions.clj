(ns conversions
  (:require [clojure.string :as str]
            [parsers :as parsers]))

;; CORRECCIÓN: Funciones de conversión Java→Map con sintaxis correcta
(defn java-ingredient-to-map [java-ingred]
  "Convierte un objeto parsers.Ingredient a map de Clojure"
  {:quantity (:quantity java-ingred)    ; Usar : para acceder a campos del record
   :unit (:unit java-ingred)           
   :text (:text java-ingred)           
   :type (:type java-ingred)})         

(defn java-instruction-to-map [java-instr]
  "Convierte un objeto parsers.Instruction a map de Clojure"
  {:step (:step java-instr)           
   :text (:text java-instr)})         

;; Tablas de conversión por tipo de ingrediente
(def conversion-table
  {:oil         218
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
   :cloves      3
   :grated      5
   :egg         50})

(def calories-per-gram
  {:oil         9.0
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
   :egg         1.6})

;; Detecta el tipo de ingrediente basado en patrones de texto
(defn detect-type-enhanced [text]
  (let [text-lower (str/lower-case (str text))]
    (cond
      (re-find #"sugar" text-lower) :sugar
      (re-find #"flour" text-lower) :flour
      (re-find #"cocoa" text-lower) :cocoa
      (re-find #"oil" text-lower) :oil
      (re-find #"butter" text-lower) :butter
      (re-find #"egg" text-lower) :egg
      (re-find #"salt|pepper|oregano|paprika" text-lower) :spice
      (re-find #"water|vinegar|juice|cream|extract" text-lower) :water-like
      (re-find #"cheese" text-lower) :cheese
      (re-find #"steak|beef|chicken|pork|fish|lamb|meat" text-lower) :meat
      (re-find #"garlic|cloves" text-lower) :cloves
      (re-find #"zest|grated" text-lower) :grated
      (re-find #"parsley|herb" text-lower) :vegetable
      :else nil)))

;; Convierte string de cantidad a número, manejando fracciones
(defn parse-amount [amount-str]
  (try
    (let [parts (str/split (str amount-str) #" ")]
      (cond
        (= (count parts) 2)
        (+ (Double/parseDouble (first parts))
           (let [[n d] (str/split (second parts) #"/")]
             (/ (Double/parseDouble n) (Double/parseDouble d))))
        (str/includes? (str amount-str) "/")
        (let [[n d] (str/split (str amount-str) #"/")]
          (/ (Double/parseDouble n) (Double/parseDouble d)))
        :else
        (Double/parseDouble (str amount-str))))
    (catch Exception e 0)))

;; Detecta ingredientes que deben mantener su unidad original
(defn should-keep-original-unit? [text unit]
  (let [text-lower (str/lower-case (str text))]
    (or
      (contains? #{"tablespoon" "teaspoon" "tbsp" "tsp"} unit)
      (re-find #"egg|huevo" text-lower)
      (= unit "pinch"))))

;; Detecta casos especiales en el texto para conversión directa
(defn detect-special-cases [text scaled]
  (let [text-lower (str/lower-case (str text))]
    (cond
      (re-find #"lbs?|pounds?" text-lower)
      {:quantity (* scaled 453.6) :unit 'g :type "detected-lbs"}
      
      (re-find #"ounces?" text-lower)
      {:quantity (* scaled 28.35) :unit 'g :type "detected-oz"}
      
      (re-find #"tbsp|tablespoons?" text-lower)
      {:quantity (* scaled 14.8) :unit 'g :type "detected-tbsp"}
      
      (re-find #"pints?" text-lower)
      {:quantity (* scaled 473.2) :unit 'g :type "detected-pint"}
      
      (re-find #"cloves?" text-lower)
      {:quantity (* scaled 3) :unit 'g :type "detected-cloves"}
      
      (re-find #"sprig" text-lower)
      {:quantity (* scaled 2) :unit 'g :type "detected-sprig"}
      
      :else nil)))

;; Detecta si debe usar conversión como items individuales
(defn should-use-item-conversion? [text unit]
  (let [text-lower (str/lower-case (str text))]
    (or 
      (and (or (nil? unit) (= unit ""))
           (or (re-find #"clove|diente" text-lower)
               (re-find #"zest|ralladura" text-lower)
               (re-find #"extract|extracto" text-lower)))
      (re-find #"zest|ralladura" text-lower))))

;; Limpia el texto del ingrediente removiendo unidades redundantes
(defn clean-ingredient-text [text unit]
  (if (and text unit (= (name unit) "g"))
    (-> text
        (str/replace #"^cups?\s+" "")          
        (str/replace #"^cup\s+" "")            
        (str/replace #"^tablespoons?\s+" "")   
        (str/replace #"^tablespoon\s+" "")     
        (str/replace #"^tbsp\.?\s+" "")        
        (str/replace #"^teaspoons?\s+" "")     
        (str/replace #"^teaspoon\s+" "")       
        (str/replace #"^tsp\.?\s+" "")         
        (str/replace #"\s+cups?\s+" " ")       
        (str/replace #"\s+cup\s+" " ")         
        (str/replace #"\s+tablespoons?\s+" " ")
        (str/replace #"\s+tablespoon\s+" " ") 
        (str/replace #"\s+tbsp\.?\s+" " ")     
        (str/replace #"\s+teaspoons?\s+" " ")  
        (str/replace #"\s+teaspoon\s+" " ")    
        (str/replace #"\s+tsp\.?\s+" " ")
        (str/replace #"\s+ounces?\s+" " ")
        (str/replace #"\s+lbs?\s+" " ")
        (str/replace #"\s+pounds?\s+" " ")
        (str/replace #"^ounces?\s+" "")
        (str/replace #"^lbs?\s+" "")
        (str/replace #"^pounds?\s+" "")      
        (str/replace #"\s+" " ")
        str/trim)
    text))

;; Estima las calorías de un ingrediente
(defn estimate-calories [ingred]
  (let [qty (:quantity ingred)
        unit (when (:unit ingred) (name (:unit ingred)))
        type (cond
               (keyword? (:type ingred)) (:type ingred)
               (string? (:type ingred)) (keyword (:type ingred))
               :else (detect-type-enhanced (:text ingred)))
        cal-per-gram (when type (get calories-per-gram type))]
    
    (let [calories (cond
                     (or (nil? type) (nil? qty) (zero? qty)) 0
                     (nil? cal-per-gram) 0
                     
                     (= unit "g") 
                     (* qty cal-per-gram)
                     
                     (and (= type :egg) (or (nil? unit) (= unit "")))
                     (let [rounded-eggs (cond
                                          (< qty 0.3) 0
                                          (< qty 0.7) 0.5
                                          (< qty 1.3) 1
                                          :else (Math/round qty))]
                       (* rounded-eggs 78))
                     
                     (contains? #{"tablespoon" "tbsp" "Tbsp"} unit)
                     (* qty 78)
                     
                     (contains? #{"teaspoon" "tsp"} unit)
                     (* qty 20)
                     
                     (= unit "cup")
                     (let [grams (get conversion-table type)]
                       (if grams
                         (* qty grams cal-per-gram)
                         0))
                     
                     :else 0)
          
          max-calories-per-ingredient 2000
          final-calories (min calories max-calories-per-ingredient)]
      
      (if (number? final-calories) (Math/round (double final-calories)) 0))))

;; Conversiones de temperatura
(defn f-to-c [f] (Math/round (double (* (- f 32) (/ 5.0 9)))))
(defn c-to-f [c] (Math/round (double (+ (* c (/ 9.0 5)) 32))))

;; Convierte diferencias de temperatura (sin restar 32)
(defn f-diff-to-c-diff [f-diff] 
  (Math/round (double (* f-diff (/ 5.0 9)))))

;; Convierte temperaturas en texto
(defn convert-temp-text [text target-unit]
  (let [pattern (get parsers/regex "temperature")
        degrees-pattern #"(\d+)(?:-(\d+))?\s+degrees?"]
    (if (or (nil? pattern) (nil? text))
      text
      (-> text
          (str/replace pattern
            (fn [[_ temp unit]]
              (let [t (Integer/parseInt temp)
                    norm-unit (str/lower-case unit)]
                (cond
                  (and (#{ "f" "f°" "fahrenheit" } norm-unit) (= target-unit "C"))
                  (str (f-to-c t) "°C")
                  (and (#{ "c" "c°" "celsius" } norm-unit) (= target-unit "F"))
                  (str (c-to-f t) "°F")
                  :else (str t "°" (str/capitalize norm-unit))))))
          (str/replace degrees-pattern
            (fn [[full-match temp1 temp2]]
              (if temp2
                (let [t1 (f-diff-to-c-diff (Integer/parseInt temp1))
                      t2 (f-diff-to-c-diff (Integer/parseInt temp2))]
                  (str t1 "-" t2 " degrees C"))
                (let [t (f-diff-to-c-diff (Integer/parseInt temp1))]
                  (str t " degrees C")))))))))

;; Escala una cantidad proporcionalmente
(defn scale-quantity [cantidad actuales nuevas]
  (if (and (number? cantidad) (number? actuales) (number? nuevas) (pos? actuales))
    (let [factor (/ nuevas actuales)]
      (* cantidad factor))
    cantidad))

;; Función principal de conversión de ingredientes
(defn convert-ingredient-struct
  [ingred {:keys [sistema porciones-actuales porciones-nuevas]}]
  (let [amount (if (string? (:quantity ingred))
                 (parse-amount (:quantity ingred))
                 (:quantity ingred))
        scaled (scale-quantity amount porciones-actuales porciones-nuevas)
        unit (when (:unit ingred) (name (:unit ingred)))
        type (if (:type ingred) 
               (keyword (:type ingred))
               (detect-type-enhanced (:text ingred)))
        text (:text ingred)]
    
    (cond
      ;; Sistema cup: mantener unidades originales o convertir g → cup
      (= sistema "cup")
      (if (and (= unit "g") (get conversion-table type))
        (let [g (get conversion-table type)
              cups (/ scaled g)
              cleaned-text (clean-ingredient-text text 'cup)]
          (assoc ingred
                 :quantity (/ (Math/round (* cups 100.0)) 100.0) 
                 :unit 'cup
                 :text cleaned-text
                 :type type))
        (assoc ingred :quantity scaled :type type))

      ;; Sistema métrico: conversión completa
      (= sistema "metric")
      (let [special-case (detect-special-cases text scaled)]
        (cond
          ;; Mantener unidades originales (tbsp, huevos)
          (should-keep-original-unit? text unit)
          (let [cleaned-text (-> text
                               (str/replace #"^cups?\s+" "")          
                               (str/replace #"^tablespoons?\s+" "")   
                               (str/replace #"^tablespoon\s+" "")     
                               (str/replace #"^tbsp\.?\s+" "")        
                               (str/replace #"^teaspoons?\s+" "")     
                               (str/replace #"^teaspoon\s+" "")       
                               (str/replace #"^tsp\.?\s+" "")
                               (str/replace #"\s+tablespoons?\s+" " ")
                               (str/replace #"\s+tablespoon\s+" " ")     
                               (str/replace #"\s+tbsp\.?\s+" " ")        
                               (str/replace #"\s+teaspoons?\s+" " ")     
                               (str/replace #"\s+teaspoon\s+" " ")       
                               (str/replace #"\s+tsp\.?\s+" " ")
                               (str/replace #"tsp\s+tsp" "")
                               (str/replace #"tbsp\s+tbsp" "")
                               (str/replace #"tablespoon\s+tablespoon" "")
                               (str/replace #"teaspoon\s+teaspoon" "")
                               (str/replace #"\s+" " ")
                               str/trim)
                final-quantity (if (or (= type :egg) (re-find #"egg" (str/lower-case text)))
                                 (cond
                                   (< scaled 0.3) 0
                                   (< scaled 0.7) 0.5
                                   (< scaled 1.3) 1
                                   :else (Math/round scaled))
                                 scaled)
                final-text (if (or (= type :egg) (re-find #"egg" (str/lower-case text)))
                             (if (= final-quantity 1)
                               (str/replace cleaned-text #"\beggs?\b" "egg")
                               (str/replace cleaned-text #"\begg\b" "eggs"))
                             cleaned-text)]
            (assoc ingred 
                   :quantity final-quantity 
                   :type type
                   :text final-text
                   :unit (cond
                           (= unit "tablespoon") 'Tbsp
                           (= unit "teaspoon") 'tsp  
                           (= unit "tbsp") 'Tbsp
                           (= unit "tsp") 'tsp
                           :else (:unit ingred))))
          
          ;; Casos especiales detectados en texto
          special-case
          (let [raw-cleaned-text (clean-ingredient-text text (:unit special-case))
                extra-cleaned-text (-> raw-cleaned-text
                                       (str/replace #"^ounces?\s+" "")
                                       (str/replace #"^lbs?\s+" "")
                                       (str/replace #"^pounds?\s+" "")
                                       (str/replace #"^pints?\s+" "")
                                       (str/replace #"^tbsp\.?\s+" "")
                                       (str/replace #"^tablespoons?\s+" "")
                                       (str/replace #"^tablespoon\s+" "")
                                       (str/replace #"\s+ounces?\s+" " ")
                                       (str/replace #"\s+lbs?\s+" " ")
                                       (str/replace #"\s+pounds?\s+" " ")
                                       (str/replace #"\s+pints?\s+" " ")
                                       (str/replace #"\s+tbsp\.?\s+" " ")
                                       (str/replace #"\s+tablespoons?\s+" " ")
                                       (str/replace #"\s+tablespoon\s+" " ")
                                       (str/replace #"Tbsp\s+" "")
                                       (str/replace #"\s+Tbsp\s+" " ")
                                       (str/replace #"Tbsp" "")
                                       (str/replace #"\s+" " ")
                                       str/trim)]
            (assoc ingred 
                   :quantity (:quantity special-case)
                   :unit (:unit special-case)
                   :text extra-cleaned-text
                   :type type))
          
          ;; Ya está en gramos, solo escalar
          (= unit "g") 
          (let [cleaned-text (clean-ingredient-text text 'g)]
            (assoc ingred 
                   :quantity (Math/round (double scaled)) 
                   :type type
                   :text cleaned-text))
          
          ;; Conversión estándar con factor conocido
          (and (get conversion-table type) 
               unit 
               (contains? #{"cup" "teaspoon" "tablespoon"} unit))
          (let [g (get conversion-table type)
                factor (case unit 
                         "cup" 1.0 
                         "teaspoon" 0.0208 
                         "tablespoon" 0.0625 
                         1.0)
                converted-grams (* scaled g factor)
                cleaned-text (clean-ingredient-text text 'g)]
            (assoc ingred 
                   :quantity (Math/round (double converted-grams))
                   :unit 'g
                   :text cleaned-text
                   :type type))
          
          ;; Conversión como items individuales
          (and (get conversion-table type) (should-use-item-conversion? text unit))
          (let [g (cond
                    (= unit "pinch") 1
                    (re-find #"zest|ralladura" (str/lower-case text)) 5
                    :else (get conversion-table type))
                converted-grams (* scaled g)
                cleaned-text (clean-ingredient-text text 'g)]
            (assoc ingred 
                   :quantity (Math/round (double converted-grams))
                   :unit 'g
                   :text cleaned-text
                   :type type))
          
          ;; Tipo conocido, asumir cups
          (and (get conversion-table type) 
               (not (should-use-item-conversion? text unit)))
          (let [g (get conversion-table type)
                converted-grams (* scaled g)
                cleaned-text (clean-ingredient-text text 'g)
                final-quantity (if (or (= type :egg) 
                                       (and text (re-find #"egg" (str/lower-case text))))
                                 (cond
                                   (< scaled 0.3) 0     
                                   (< scaled 0.7) 0.5   
                                   (< scaled 1.3) 1     
                                   :else (Math/round scaled))
                                 (Math/round (double converted-grams)))
                final-text (if (or (= type :egg) 
                                   (and text (re-find #"egg" (str/lower-case text))))
                             (if (= final-quantity 1)
                               (str/replace cleaned-text #"\beggs?\b" "egg")
                               (str/replace cleaned-text #"\begg\b" "eggs"))
                             cleaned-text)
                final-unit (if (or (= type :egg) 
                                   (and text (re-find #"egg" (str/lower-case text))))
                             nil
                             'g)]
            (assoc ingred 
                   :quantity final-quantity
                   :unit final-unit
                   :text final-text
                   :type type))
          
          ;; Sin tipo conocido, mantener original con limpieza
          :else 
          (let [cleaned-text (if text 
                               (-> text
                                   (str/replace #"^cups?\s+" "")          
                                   (str/replace #"^tablespoons?\s+" "")   
                                   (str/replace #"^tablespoon\s+" "")     
                                   (str/replace #"^tbsp\.?\s+" "")        
                                   (str/replace #"^teaspoons?\s+" "")     
                                   (str/replace #"^teaspoon\s+" "")       
                                   (str/replace #"^tsp\.?\s+" "")
                                   (str/replace #"^ounces?\s+" "")
                                   (str/replace #"^lbs?\s+" "")
                                   (str/replace #"^pounds?\s+" "")
                                   (str/replace #"^pints?\s+" "")
                                   (str/replace #"\s+tablespoons?\s+" " ")
                                   (str/replace #"\s+tablespoon\s+" " ")     
                                   (str/replace #"\s+tbsp\.?\s+" " ")        
                                   (str/replace #"\s+teaspoons?\s+" " ")     
                                   (str/replace #"\s+teaspoon\s+" " ")       
                                   (str/replace #"\s+tsp\.?\s+" " ")
                                   (str/replace #"\s+ounces?\s+" " ")
                                   (str/replace #"\s+lbs?\s+" " ")
                                   (str/replace #"\s+pounds?\s+" " ")
                                   (str/replace #"\s+pints?\s+" " ")
                                   (str/replace #"\s+" " ")
                                   str/trim)
                               text)
                final-quantity (if (or (= type :egg) 
                                       (and text (re-find #"egg" (str/lower-case text))))
                                 (cond
                                   (< scaled 0.3) 0     
                                   (< scaled 0.7) 0.5   
                                   (< scaled 1.3) 1     
                                   :else (Math/round scaled))
                                 scaled)
                final-text (if (or (= type :egg) 
                                   (and text (re-find #"egg" (str/lower-case text))))
                             (if (= final-quantity 1)
                               (str/replace cleaned-text #"\beggs?\b" "egg")
                               (str/replace cleaned-text #"\begg\b" "eggs"))
                             cleaned-text)]
            (assoc ingred :quantity final-quantity :type type :text final-text))))
      
      ;; Sistema desconocido
      :else 
      (assoc ingred :quantity scaled :type type))))

;; Agrega información de calorías al metadata de la receta
(defn update-metadata-with-calories [recipe]
  (let [ingredientes-validos (filter #(some? (:quantity %)) (:ingredients recipe))
        calorias-lista (map estimate-calories ingredientes-validos)
        total-cal (reduce + 0 calorias-lista)
        servings (:servings recipe)
        cal-per-serving (if (and servings (pos? servings))
                          (Math/round (double (/ total-cal servings)))
                          0)]
    
    (update recipe :metadata assoc 
            :calories (Math/round (double total-cal))
            :calories-per-serving cal-per-serving)))

;; Analiza una instrucción buscando temperaturas e ingredientes
(defn analyze-instruction [instr ingredients target-temp-unit]
  (let [text (:text instr)
        converted-text (convert-temp-text text target-temp-unit)
        ingredient-names (map #(str/lower-case (:text %)) ingredients)
        found-ingreds (filter #(str/includes? (str/lower-case converted-text) %) ingredient-names)]
    (assoc instr
           :text converted-text
           :contains-temp? (not= converted-text text)
           :contains-ingredient? (not (empty? found-ingreds)))))

;; Analiza todas las instrucciones de una receta
(defn analyze-instructions [recipe]
  (let [analyzed (map #(analyze-instruction % (:ingredients recipe)
                                            (name (:temperature-unit recipe)))
                      (:instructions recipe))]
    (assoc recipe :instructions analyzed)))

;; FUNCIÓN PRINCIPAL CORREGIDA: convert-recipe
(defn convert-recipe
  [recipe {:keys [sistema porciones-actuales porciones-nuevas temperature-unit]}]
  
  ;; PASO 1: Convertir ingredientes e instrucciones de Java objects a maps
  (let [ingredients-as-maps (map java-ingredient-to-map (:ingredients recipe))
        instructions-as-maps (map java-instruction-to-map (:instructions recipe))
        
        ;; PASO 2: Crear receta temporal con maps
        recipe-with-maps (assoc recipe 
                               :ingredients ingredients-as-maps
                               :instructions instructions-as-maps)
        
        ;; PASO 3: Aplicar conversiones a los maps
        converted-ingredients
        (map #(convert-ingredient-struct %
                 {:sistema sistema
                  :porciones-actuales porciones-actuales
                  :porciones-nuevas porciones-nuevas})
             ingredients-as-maps)

        ;; PASO 4: Construir receta final
        updated-recipe
        (assoc recipe-with-maps
               :ingredients converted-ingredients
               :servings porciones-nuevas
               :temperature-unit (keyword temperature-unit))

        recipe-with-calories
        (update-metadata-with-calories updated-recipe)

        final-recipe
        (analyze-instructions recipe-with-calories)]

    final-recipe))