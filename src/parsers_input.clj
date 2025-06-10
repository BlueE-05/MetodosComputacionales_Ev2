(ns parsers_input)

(def regex {
  "measures"      #"^sistema:\s*(\w+)"
  "temp"          #"^temp:\s*(\w+)"
  "servings"      #"^porciones:\s*(\d+)"
  "recipe_type"   #"^filtra:\s*(\w+)"
})

(defn match-line [line]
  (some (fn [[k r]]
          (when-let [m (re-matches r line)]
            (let [[_ value] m]
              [k value])))
        regex))

; --función que devuelve un mapa con claves: :measures, :temp, :servings, :recipe_type--
(defn parser-input [lines]
  (into {} 
        (keep match-line lines)))

