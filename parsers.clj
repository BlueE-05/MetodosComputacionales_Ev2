(ns parsers
  (:require [clojure.string :as str]))

;; REGEX DICTIONARY
(def regex {
  ; recipe general data
  "author"     #"^(?:Author|Submitted by|By):\s*(.+)$"
  "servings"   #"^(?:Servings|serves|Servings -|servings -)\s*[:\-]?\s*(\d+)$"
  "prep-time"  #"^(?:Prep Time|Preparation Time):\s*(.+)$"
  "cook-time"  #"^(?:Cook Time|Cooking Time):\s*(.+)$"
  "total-time" #"^(?:Total Time):\s*(.+)$"
  "category"   #"^Category:\s*(.+)$"
  "ingredients"  #"^(Ingredients:|Ingredients|Ingredient List:|Ingredients List:|Ingredients -)"
  "instructions" #"^(Instructions:|Instructions|Method:|Directions:|How to Prepare:|Preparation Steps:)"
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
  "temperature" #"(?i)(\d+)\s*°?\s*(C|F)"
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
  "grated"     #"(?i)\bgrated zest\b"
  "egg"        #"(?i)\b(egg|eggs)\b"
  ; numbers
  "number"     #"^\d+(\.\d+)?"
  "fraction"   #"^\d+/\d+"
})

;; STRUCTS
(defrecord Recipe [category servings unit-system temperature-unit analysis-time tokens metadata ingredients instructions])
(defrecord Ingredient [quantity unit text type])
(defrecord Instruction [step text contains-temp? contains-ingredient?])

;; UTILITIES
; --función que 
(defn match-line [line]
  (some (fn [[k r]]
          (when-let [m (re-matches r line)]
            [k (last m)]))
        regex))

; --función que 
(defn matches-any-regex? [line]
  (some (fn [[_ r]] (re-matches r line)) regex))

; --función que divide las secciones de la receta--
(defn split-sections [lines]
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
  (reduce
    (fn [acc line]
      (if-let [[k v] (match-line line)]
        (assoc acc (keyword k)
               (try (Integer/parseInt v)
                    (catch Exception _ v)))
        acc))
    {} lines))

; --función que obtine descripción de la receta o cosas que no pertenecen a ninguna otra sección--
(defn parser-tokens [lines]
  (let [[title & rest] lines
        author-index (first (keep-indexed
                             (fn [i line]
                               (when (re-matches (get regex "author") line) i))
                             rest))
        before-author (if author-index (subvec (vec rest) 0 author-index) rest)
        author-line (when author-index (nth rest author-index))
        author-name (when-let [m (re-matches (get regex "author") author-line)]
                      (second m))
        after-author (if author-index (subvec (vec rest) (inc author-index)) []) 
        description-parts (concat before-author
                                  (take-while #(not (matches-any-regex? %)) after-author))
        description (str/join " " (filter (complement str/blank?) description-parts))]

    (merge
     {:title title}
     (when (not (str/blank? description)) {:description description})
     (when author-name {:author author-name}))))

; --función que extrae cantidades--
(defn parse-quantity [word]
  (try ; try catch para evitar crasheos
    (cond
      (re-matches (get regex "number") word) (Double/parseDouble word)
      (re-matches (get regex "fraction") word)
      (let [[n d] (str/split word #"/")]
        (/ (Double/parseDouble n) (Double/parseDouble d)))
      :else nil)
    (catch Exception _ nil)))

; --función que !
(defn extract-quantity [words]
  (some parse-quantity words))

; --función que detecta las unidades de medición --
(defn detect-unit [line]
  (some (fn [[k r]]
          (when (and (contains? #{"cup" "teaspoon" "tablespoon" "grams" "kilograms"
                                  "liters" "ml" "pinch"} k)
                     (re-find r line))
            (symbol k)))
        regex))

; --función que detecta el tipo de ingrediente-- !
(defn detect-type [ingredient-name]
  (some (fn [[k r]]
          (when (re-find r (str/lower-case ingredient-name))
            (symbol k)))
        regex))

; -- !
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
	
;; -- Parsing Instruction --
(defn parse-temperature [line]
  (when-let [m (re-find (get regex "temperature") line)]
    [(Double/parseDouble (nth m 1))
     (if (= (str/upper-case (nth m 2)) "F") 'F 'C)]))

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

(defn infer-category-from-title [title]
  (cond
    (re-find (get regex "side-dish") title) "side-dish"
    (re-find (get regex "dessert") title) "dessert"
    :else "all"))

;; -- Final recipe builder --
(defn build-recipe [lines]
  (let [tokens-map (parser-tokens lines)
        metadata-map (parser-metadata lines)
        sections (split-sections lines)
        ingredients (parser-ingredient (get sections :ingredients []))
        instructions (parser-instructions (get sections :instructions []))
        title-category (infer-category-from-title (:title tokens-map))
        final-category (keyword (or (:category metadata-map) title-category))]

    (->Recipe 
      final-category
      (get metadata-map :servings 1)
      'cup
      'Celsius
      5
      tokens-map
      metadata-map
      ingredients
      instructions)))

;; -- Example test --
(def example-lineas
  ["Chimichurri Sauce"
   "This Pan-Seared Steak has a garlic butter that makes it taste like a steakhouse quality meal."
   "You'll be impressed at how easy it is to make the perfect steak that's seared on the outside, and perfectly tender inside."
   "Author: Natasha of NatashasKitchen.com"
   "Servings - 4"
   "Prep Time: 5 mins"
   "Cook Time: 15 mins"
   "Total Time: 20mins"
   "Ingredients"
   "2 tablespoons olive oil"
   "1 teaspoon salt"
   "1/2 cup extra-virgin olive oil"
   "Instructions:"
   "1. Season the steak with salt and pepper."
   "2. Heat the oil in a skillet over high heat."
   "3. Preheat the oven to 325°F."
   "4. Thoroughly pat steak dry with paper towels. Just before cooking, generously season with 1 1/2 tsp salt and 1 tsp black pepper"])

(println (build-recipe example-lineas))