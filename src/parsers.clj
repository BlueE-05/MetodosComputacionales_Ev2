(ns parsers
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; REGEX DICTIONARY
(def regex {
  ; recipe general data
  "author"     #"^(?:Author|Submitted by|By|From):\s*(.+)$"
  "servings"   #"(?i).*(?:servings|serves)\s*[-:]?\s*(\d+).*"
  "prep-time"  #"^(?:Prep Time|Preparation Time):\s*(.+)$"
  "cook-time"  #"^(?:Cook Time|Cooking Time):\s*(.+)$"
  "total-time" #"^(?:Total Time):\s*(.+)$"
  "category"   #"^Category:\s*(.+)$"
  "ingredients"  #"^(Ingredients:|Ingredients|Ingredients: |Ingredient List:|Ingredients List:|Ingredients -)"
  "instructions" #"^(Instructions:|Instructions|Instructions |Method:|Directions:|How to Prepare:|Preparation Steps:)"
  ; category keywords
  "side-dish"  #"(?i)\b(Side Dish|Sauce|Compliment|Sauces|Dressing)\b"
  "dessert"    #"(?i)\b(Cake|Brownie|Brownies|Cookies)\b"
  ; units
  "cup"        #"\b\d+(?:\.\d+)?\s+(cup|cups)\b"
  "teaspoon"   #"\b\d+(?:\.\d+)?\s+(teaspoon|teaspoons|tsp)\b"
  "tablespoon" #"\b\d+(?:\.\d+)?\s+(tablespoon|tablespoons|tbsp)\b"
  "grams"      #"\b\d+(?:\.\d+)?\s+(grams|gram|g)\b"
  "kilograms"  #"\b\d+(?:\.\d+)?\s+(kilograms|kg)\b"
  "liters"     #"\b\d+(?:\.\d+)?\s+(liters|liter|l)\b"
  "ml"         #"\b\d+(?:\.\d+)?\s+(milliliters|milliliter|ml)\b"
  "pinch"      #"\b(?:dash|pinch)\b"
  ; temperature
  "temperature" #"(?i)(\d+)\s*°?\s*(C|F|celsius|fahrenheit)"
  ; ingredient types
  "oil"        #"(?i)\boil\b"
  "spice"      #"(?i)\b(spice|paprika|cumin|cinnamon|pepper|salt|oregano|rosemary)\b"
  "vegetable"  #"(?i)\b(vegetable|onion|garlic|carrot|celery|herb|parsley|basil|cilantro|thyme)\b"
  "flour"      #"(?i)\bflour\b"
  "baking_soda" #"(?i)\b(baking soda|baking powder)\b"
  "cocoa"      #"(?i)\bcocoa powder\b"
  "water-like" #"(?i)\b(vinegar|water|broth|stock|juice|milk|sauce|extract|lemon juice)\b"
  "sugar"      #"(?i)\bgranulated sugar\b"
  "sugar_powder" #"(?i)\bpowdered sugar\b"
  "butter"     #"(?i)\bbutter\b"
  "cheese"     #"(?i)\bcheese\b"
  "meat"       #"(?i)\b(beef|chicken|pork|fish|lamb)\b"
  "fruit"      #"(?i)\b(apple|banana|orange|lemon|berry)\b"
  "cloves"     #"(?i)\b(cloves|sprig)\b"
  "lemon_zest" #"(?i)\bgrated zest\b"
  "egg"        #"(?i)\b(egg|eggs)\b"
  ; numbers
  "number"     #"^\d+(\.\d+)?"
  "fraction"   #"^\d+/\d+"
})

;; STRUCTS
(defrecord Recipe [category servings unit-system temperature-unit tokens metadata ingredients instructions]) ;maybe add :analysis time
(defrecord Ingredient [quantity unit text type])
(defrecord Instruction [step text contains-temp? contains-ingredient?])

;; UTILITIES
; --función que regresa la primer coincidencia de la linea con el regex o nil en el formato [clave último-grupo]--
(defn match-line [line]
  (some (fn [[k r]]
          (when-let [m (re-matches r line)] ; rematches toma un regex y una cadena de texto. Devuelve la coincidencia completa si toda la cadena coincide exactamente o nil
            [k (last m)]))
        regex))

; --función que regresa la primer coincidencia de la linea con el regex o nil--
(defn matches-any-regex? [line]
  (some (fn [[_ r]] (re-matches r line)) regex))

; --función que divide las secciones de la receta--
(defn split-sections [lines]
  ; Encuentra los índices donde comienzan las secciones de ingredientes e instrucciones
  (let [idx-map (reduce (fn [acc [i line]]
                          (cond
                            (re-matches (regex "ingredients") line) (assoc acc :ingredients i)
                            (re-matches (regex "instructions") line) (assoc acc :instructions i)
                            :else acc))
                        {} (map-indexed vector lines))
        ingredients-lines (when-let [start (:ingredients idx-map)]
                            (let [end (or (:instructions idx-map) (count lines))] 
                              (subvec lines (inc start) end)))
        instructions-lines (when-let [start (:instructions idx-map)]
                             (subvec lines (inc start)))]

    {:ingredients ingredients-lines
     :instructions instructions-lines}))

;; PARSERS
; --función que obtiene la metadata de la receta--
(defn parser-metadata [lines]
  (reduce ; acumular resultados en un mapa vacío
    (fn [acc line]
      (if-let [[k v] (match-line line)]
        (assoc acc (keyword k)
               (try (Integer/parseInt v) ; Intenta convertir el valor v a entero con Integer/parseInt
                    (catch Exception _ v)))
        acc))
    {} lines))

; --función que obtine descripción de la receta o cosas que no pertenecen a ninguna otra sección--
(defn parser-tokens [lines]
  (let [lines (vec (filter (comp not str/blank?) lines))
        title (first lines)
        rest-lines (subvec lines 1)
        section-index (first (keep-indexed
                               (fn [i line]
                                 (when (or (re-matches (get regex "ingredients") line)
                                           (re-matches (get regex "instructions") line))
                                   i))
                               rest-lines))
        before-section (if section-index (subvec rest-lines 0 section-index) rest-lines)
        author-line (some #(when-let [m (re-matches (get regex "author") %)] m) before-section)
        author-name (when author-line (second author-line))
        description-lines (remove #(re-matches (get regex "author") %) before-section)
        description (str/join " " (filter (complement str/blank?) description-lines))]
    (merge
     {:title title}
     (when (not (str/blank? description)) {:description description})
     (when author-name {:author author-name}))))

; --función que extrae cantidades--
(defn parse-quantity [word]
  (try ; try catch para evitar crasheos
    (cond
      (re-matches (get regex "number") word) (Double/parseDouble word) ; convierte word a un número con punto decimal con Double/parseDouble
      (re-matches (get regex "fraction") word)
      (let [[n d] (str/split word #"/")]
        (/ (Double/parseDouble n) (Double/parseDouble d)))
      :else nil)
    (catch Exception _ nil)))

; --función que busca y extrae la primera cantidad válida en una lista de palabras--
(defn extract-quantity [words]
  (some parse-quantity words)) ; some aplica parse-quantity a cada elemento y devuelve el primer resultado no-nil

; --función que detecta las unidades de medición 'metric o 'cup--
(defn detect-unit [line]
  (some (fn [[k r]]
          (when (and (contains? #{"cup" "teaspoon" "tablespoon" "grams" "kilograms" "liters" "ml" "pinch"} k)
                     (re-find r line))
            (symbol k)))
        regex))

; --función que detecta el tipo de ingrediente-- !
(defn detect-type [ingredient-name]
  (some (fn [[k r]]
          (when (re-find r (str/lower-case ingredient-name))
            (symbol k)))
        regex))

; --función que lee los ingredientes de lines y crea el ingrediente--
(defn parser-ingredient [lines]
  (letfn [(parse-lines [ls acc]
            (if (empty? ls)
              acc
              (let [line (first ls)]
                (if (matches-any-regex? line)
                  (parse-lines (rest ls) acc) ; omitir encabezados como "Ingredients:"
                  (let [words (str/split line #"\s+")
                        quantity (extract-quantity words)
                        unit (detect-unit line)
                        skip (cond
                               (and quantity unit) 2
                               quantity 1
                               :else 0)
                        text (->> words (drop skip) (str/join " "))
                        type (detect-type text)]
                    (parse-lines
                      (rest ls)
                      (conj acc (->Ingredient quantity unit text type))))))))]
    (parse-lines lines [])))
	
; --función que identifica F|C--
(defn parse-temperature [line]
  (when-let [m (re-find (get regex "temperature") line)]
    [(Double/parseDouble (nth m 1))
     (if (= (str/upper-case (nth m 2)) "F") 'F 'C)]))

; --función que lee y crea la instrucción a partir de line--
(defn parse-instruction-line [line step-num]
  (let [clean-line (str/trim (str/replace line #"^\d+\.\s*" ""))
        contains-temp? (boolean (parse-temperature line))
        contains-ingredient?
        (some (fn [[_ r]]
                (when (and (re-find r line)
                           (contains? #{"cup" "teaspoon" "tablespoon" "grams" "kilograms" "liters" "ml" "pinch"} _))
                  true))
              regex)]
    (->Instruction
      step-num
      clean-line
      contains-temp?
      (boolean contains-ingredient?))))

; -- --
(defn parser-instructions
  ([lines] (parser-instructions lines 1 []))
  ([lines step-num acc]
   (cond
     (empty? lines) acc
     (str/blank? (first lines)) (parser-instructions (rest lines) step-num acc)
     :else
     (let [line (first lines)
           inst (parse-instruction-line line step-num)]
       (parser-instructions (rest lines) (inc step-num) (conj acc inst))))))

;; INFERS
(defn infer-category-from-title [title]
  (cond
    (re-find (get regex "side-dish") title) "side-dish"
    (re-find (get regex "dessert") title) "dessert"
    :else "all"))

(defn infer-unit-system [ingredients]
  (let [metric? (some #(contains? #{'grams 'kilograms 'liters 'ml} (:unit %)) ingredients)
        imperial? (some #(contains? #{'cup 'teaspoon 'tablespoon 'pinch} (:unit %)) ingredients)]
    (cond
      metric? 'metric
      imperial? 'cup
      :else 'unknown)))

(defn infer-temperature-unit [instructions]
  (let [temps (keep #(parse-temperature (:text %)) instructions)
        units (set (map second temps))]
    (cond
      (units 'F) 'F
      (units 'C) 'C
      :else nil)))

;; FINAL
; --función que crea la receta, aplicando todas las funciones previas--
(defn build-recipe [lines]
  (let [tokens-map (parser-tokens lines)
        metadata-map (parser-metadata lines)
        sections (split-sections lines)
        ingredients (parser-ingredient (get sections :ingredients []))
        instructions (parser-instructions (get sections :instructions []))
        title-category (infer-category-from-title (:title tokens-map))
        final-category (keyword (or (:category metadata-map) title-category))
        unit-system (infer-unit-system ingredients)
        temp-unit (infer-temperature-unit instructions)]

    (->Recipe 
      final-category
      (get metadata-map :servings 1)
      unit-system
      temp-unit
      tokens-map
      metadata-map
      ingredients
      instructions)))

(defn leer-archivos-de-receta [ruta-carpeta]
  (->> (file-seq (io/file ruta-carpeta))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".txt")))
       (map #(-> % slurp str/split-lines parsers/build-recipe))
       (remove nil?)
       vec))

;(def lines {
;  "HELLO"
;})
;
;(build-recipe lines)