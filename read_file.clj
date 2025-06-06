(defn leer-lineas [ruta-archivo]
  (clojure.string/split-lines (slurp ruta-archivo)))

(doseq [parrafo (leer-lineas "recipe_collection/Chimichurri Sauce.txt")] (println parrafo))
