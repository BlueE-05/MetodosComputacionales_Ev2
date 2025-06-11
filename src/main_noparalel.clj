(ns main-noparalel
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [parsers :as parsers]
            [parsers-input :as parsers-input]
            [conversions :as conversions]
            [create-html :as create-html]
            [create-styles :as create-styles]))

;; Lee y parsea el archivo de configuración
(defn leer-configuracion [ruta-options]
  (let [config (-> ruta-options
                   slurp
                   str/split-lines
                   parsers-input/parser-input)
        config (into {} (map (fn [[k v]] [(keyword k) v]) config))]
    (-> config
        (assoc :measures (or (:measures config) (:sistema config)))
        (assoc :servings (or (:servings config) (:porciones config)))  
        (assoc :recipe_type (or (:recipe_type config) (:filtra config)))
        (update :measures #(when % (keyword %)))
        (update :temp #(when % (keyword %)))
        (update :servings #(cond
                             (string? %) (Integer/parseInt %)
                             (number? %) %
                             :else 4))
        (update :recipe_type #(when % (str/lower-case %))))))

;; Lee todos los archivos .txt de recetas
(defn leer-archivos-de-receta [ruta-carpeta]
  (->> (file-seq (io/file ruta-carpeta))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".txt")
                     (not (str/starts-with? (.getName %) "options"))))
       (map #(-> % slurp str/split-lines parsers/build-recipe))
       (filter some?)
       vec))

;; Filtra recetas por categoría
(defn filtrar-recetas [recetas filtro]
  (if (= filtro "all")
    recetas
    (filter #(= (str/lower-case (name (:category %))) filtro) recetas)))

;; Filtra ingredientes inválidos
(defn limpiar-ingredientes [ingredientes]
  (filter #(and (some? (:quantity %))
                (some? (:text %))
                (not (str/blank? (str (:text %)))))
          ingredientes))

;; Procesa una receta aplicando conversiones
(defn procesar-receta [receta config]
  (let [sistema (name (:measures config))
        nueva-temp (name (:temp config))
        porciones-nuevas (:servings config)
        porciones-actuales (:servings receta)]
    
    (conversions/convert-recipe
      receta
      {:sistema sistema
       :porciones-actuales porciones-actuales
       :porciones-nuevas porciones-nuevas
       :temperature-unit nueva-temp})))

;; Crea la carpeta si no existe
(defn asegurar-carpeta [ruta]
  (let [dir (io/file ruta)]
    (when-not (.exists dir)
      (.mkdirs dir))))

;; Función principal del sistema
(defn -main []
  (println "=== SISTEMA DE PROCESAMIENTO DE RECETAS ===")
  
  (reset! create-html/recipe-counter 0)
  
  (let [ruta-options "input_testing/options1.txt"
        ruta-recetas "recipe_collection"
        carpeta-salida "results"]
    
    (asegurar-carpeta carpeta-salida)
    
    (let [config (leer-configuracion ruta-options)]
      (println (str "Configuración: " (name (:measures config)) 
                   " | " (name (:temp config)) 
                   " | " (:servings config) " porciones"
                   " | filtro: " (:recipe_type config)))
      
      (let [recetas-originales (leer-archivos-de-receta ruta-recetas)]
        
        (if (empty? recetas-originales)
          (println "ERROR: No se encontraron recetas")
          (do
            (println (str "Recetas encontradas: " (count recetas-originales)))
            
            (let [recetas-filtradas (filtrar-recetas recetas-originales (:recipe_type config))]
              (println (str "Recetas después del filtro '" (:recipe_type config) "': " 
                           (count recetas-filtradas)))
              
              (if (empty? recetas-filtradas)
                (println "No hay recetas que coincidan con el filtro")
                (do
                  (create-styles/create-styles-file)
                  
                  (println "Procesando recetas...")
                  (doseq [receta recetas-filtradas]
                    (let [titulo (get-in receta [:tokens :title] "Sin título")
                          receta-limpia (update receta :ingredients limpiar-ingredientes)
                          receta-procesada (procesar-receta receta-limpia config)]
                      
                      (println (str "  - Procesando: " titulo))
                      (create-html/create-html-file receta-procesada)))
                  
                  (Thread/sleep 500)
                  
                  (let [files-count (create-html/verify-files)]
                    (println (str "RESULTADO FINAL: " files-count " archivos HTML generados exitosamente")))
                  
                  (println "Procesamiento completado exitosamente")))))))))

  (println "=== FIN DEL PROCESAMIENTO ==="))

;; Ejecutar la función principal si este archivo es ejecutado directamente
(when (= *ns* 'main-noparalel)
  (-main))