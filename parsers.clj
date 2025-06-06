(ns parsers
  (:require [clojure.string :as str]))

; --función para leer líneas de un archivo--
(defn leer-lineas [ruta-archivo]
  (clojure.string/split-lines (slurp ruta-archivo)))

; --diccionario de expresiones regulares para tokenizar la receta--
(def regex{
   "author"     #"^(Author|Submitted by|By):\s*(.+)$"
   "servings"   #"^(Servings|serves|Servings -|servings -)\s*[:\-]?\s*(\d+)$"
   "prep-time"  #"^(Prep Time|Preparation Time):\s*(.+)$"
   "cook-time"  #"^(Cook Time|Cooking Time):\s*(.+)$"
   "total-time" #"^(Total Time):\s*(.+)$"
   "category"   #"^Category:\s*(.+)$"
   "ingredients" #"^Ingredients:|^Ingredient List:|^Ingredients List:|^Ingredients -"
   "instructions" #"^Instructions:|^Method:|^Directions:|^How to Prepare:|^Preparation Steps:"

   "cup"        #"^\d+\s+(cup|cups)(.+)$"
   "teaspoon"   #"^\d+\s+(teaspoon|teaspoons|tsp)(.+)$"
   "tablespoon" #"^\d+\s+(tablespoon|tablespoons|tbsp)(.+)$"
   "grams"      #"^\d+\s+(grams|gram|g)(.+)$"
   "kilograms"  #"^\d+\s+(kilograms|kg)(.+)$"
   "liters"     #"^\d+\s+(liters|liter|l)(.+)$"
   "ml"         #"^\d+\s+(milliliters|milliliter|ml)(.+)$"
   "pinch"      #"^\d+\s+(dash|pinch)(.+)$"

   "oil"        #"^oil"
   "spice"      #"^(spice|paprika|cumin|cinnamon|pepper|salt|oregano)(.+)$"
   "vegetable"  #"^(vegetable|onion|garlic|carrot|celery|herb|parsley|basil|cilantro|thyme)(.+)$"
   "flour"      #"^flour"
   "baking soda" #"^()"
   "cocoa"      #"^cocoa powder"
   "water-like" #"^vinegar|water|broth|stock|juice|milk|sauce|extract"
   "sugar"      #"^sugar"
   "butter"     #"^butter"
   "cheese"     #"^cheese"
   "meat"       #"^(beef|chicken|pork|fish|lamb)(.+)$"
   "fruit"      #"^(apple|banana|orange|lemon|berry)(.+)$"
   "cloves"     #"^(cloves|sprig)"

   "number"     #"(\d+|\d+/\d+|\d+\.\d+|\d+)"
})

; --función para encontrar coincidencias con las expresiones regulares--
(defn match-line [line]
  (some (fn [[k r]]
          (when-let [m (re-matches r line)]
            [k (last m)]))
        regex))

; --función para verificar si una línea coincide con alguna expresión regular--
(defn matches-any-regex? [line]
  (some (fn [k] (re-matches (get regex k) line)) (keys regex)))

;; PARSERS
; --parser de metadata: extrae porciones, tiempos, categoría--
(defn parser-metadata [lines]
  (reduce
   (fn [acc line]
     (if-let [[k v] (match-line line)]
       (assoc acc (keyword k) v)
       acc))
   {} lines))

; --parser de tokens: extrae título, descripción, autor--
(defn parser-tokens [lines]
  (let [[title & rest] lines
        author-index (first (keep-indexed
                             (fn [i line]
                               (when (re-matches (get regex "author") line) i))
                             rest))
        before-author (if author-index
                        (subvec (vec rest) 0 author-index)
                        rest)
        author-line (when author-index
                      (nth rest author-index))
        after-author (if author-index
                       (subvec (vec rest) (inc author-index))
                       [])
        ;; Descripciones separadas antes y después del autor
        description-before (when (seq before-author)
                             (str/join " " before-author))
        description-after (when (seq after-author)
                            (let [desc-after (take-while #(not (matches-any-regex? %)) after-author)]
                              (when (seq desc-after)
                                (str/join " " desc-after))))]
    (merge
     {:title title}
     (when (not (str/blank? description-before))
       {:description description-before})
     (when author-line
       {:author author-line})
     (when (not (str/blank? description-after))
       {:description description-after}))))

(defn parser-ingredients [lines]
  (let [ingredient-regex #"^\d+\s+\w+\s+.+$"] ; Regex simple para ingredientes
    (filter #(re-matches ingredient-regex %) lines)))

(defn parser-instructions [lines]
  (let [instruction-regex #"^\d+\.\s+.+$"] ; Regex simple para instrucciones
    (filter #(re-matches instruction-regex %) lines)))

; --función principal que procesa las líneas y devuelve los tokens--
(defn parsers [lines]
  {:tokens (parser-tokens lines)
   :metadata (parser-metadata lines)}
)

;; Ejemplo
(def example-lineas
  '("Pan-Seared Steak with Garlic Butter"
    "This Pan-Seared Steak has a garlic butter that makes it taste like a steakhouse quality meal."
    "You'll be impressed at how easy it is to make the perfect steak that's seared on the outside, and perfectly tender inside."
    "Author: Natasha of NatashasKitchen.com"
    "Servings - 4"
    "Prep Time: 5 mins"
    "Cook Time: 15 mins"
    "Total Time: 20mins"))

(println "Tokens:" (parsers example-lineas))
(println "Metadata:" (parsers example-lineas))