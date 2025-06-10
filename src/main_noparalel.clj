(ns main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [parsers :as parsers]
            [parsers-input :as parsers-input]
            [conversions :as conversions]
            [create-html :as create-html]
            [create-styles :as create-styles]))

;; Lectura de configuración
(defn leer-configuracion [ruta-options]
  "Lee y parsea el archivo de configuración - Compatible con nombres en español e inglés"
  (let [config (-> ruta-options
                   slurp
                   str/split-lines
                   parsers-input/parser-input)
        config (into {} (map (fn [[k v]] [(keyword k) v]) config))]
    (-> config
        ;; Mapear nombres en español a nombres internos en inglés
        (assoc :measures (or (:measures config) (:sistema config)))
        (assoc :servings (or (:servings config) (:porciones config)))  
        (assoc :recipe_type (or (:recipe_type config) (:filtra config)))
        ;; Procesar los valores mapeados
        (update :measures #(when % (keyword %)))
        (update :temp #(when % (keyword %)))
        (update :servings #(cond
                             (string? %) (Integer/parseInt %)
                             (number? %) %
                             :else 4))
        (update :recipe_type #(when % (str/lower-case %))))))

(defn leer-archivos-de-receta [ruta-carpeta]
  "Lee todos los archivos .txt de recetas de la carpeta especificada"
  (let [archivos (->> (file-seq (io/file ruta-carpeta))
                      (filter #(and (.isFile %)
                                    (str/ends-with? (.getName %) ".txt")
                                    (not (str/starts-with? (.getName %) "options"))))
                      vec)]
    
    (->> archivos
         (map #(-> % slurp str/split-lines parsers/build-recipe))
         (filter some?)
         vec)))

;; Filtrado y limpieza
(defn filtrar-recetas [recetas filtro]
  "Filtra recetas por categoría"
  (if (= filtro "all")
    recetas
    (filter #(= (name (:category %)) filtro) recetas)))

(defn limpiar-ingredientes [ingredientes]
  "Filtra ingredientes que no tienen cantidad o texto válido"
  (filter #(and (some? (:quantity %))
                (some? (:text %))
                (not (str/blank? (str (:text %)))))
          ingredientes))

;; Procesamiento paralelo
(defn procesar-receta [receta config]
  "Procesa una receta individual aplicando todas las conversiones"
  (let [sistema (when (:measures config) (name (:measures config)))
        nueva-temp (when (:temp config) (name (:temp config)))
        porciones-nuevas (:servings config)
        porciones-actuales (:servings receta)]
    
    (conversions/convert-recipe
      receta
      {:sistema sistema
       :porciones-actuales porciones-actuales
       :porciones-nuevas porciones-nuevas
       :temperature-unit nueva-temp})))

(defn procesar-recetas-paralelo [recetas config]
  "Procesa recetas en paralelo"
  (vec (pmap #(procesar-receta % config) recetas)))

;; Generación de HTML
(defn asegurar-carpeta [ruta]
  "Crea la carpeta si no existe"
  (let [dir (io/file ruta)]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn generar-html-para-recetas [recetas]
  "Genera archivos HTML para todas las recetas"
  (create-styles/create-styles-file)
  (doseq [receta recetas]
    (create-html/create-html-file receta)))

;; Logging básico
(defn imprimir-estadisticas [config recetas-originales recetas-transformadas]
  "Muestra estadísticas del procesamiento"
  (println "=== PROCESAMIENTO DE RECETAS COMPLETADO ===")
  (println (str "Configuración: " (name (:measures config)) " | " 
               (name (:temp config)) " | " (:servings config) " porciones"))
  (println (str "Recetas procesadas: " (count recetas-transformadas)))
  (println (str "Archivos HTML generados: " (count recetas-transformadas)))
  
  (let [calorias-totales (reduce + (map #(get-in % [:metadata :calories] 0) recetas-transformadas))
        promedio-calorias (if (pos? (count recetas-transformadas))
                           (/ calorias-totales (count recetas-transformadas))
                           0)]
    (println (str "Calorías totales procesadas: " (int calorias-totales) " kcal"))
    (println (str "Promedio de calorías por receta: " (int promedio-calorias) " kcal"))))

;; Función principal
(defn -main []
  (println "=== SISTEMA DE PROCESAMIENTO DE RECETAS ===")
  
  (let [ruta-options "input_testing/options1.txt"
        ruta-recetas "recipe_collection"
        carpeta-salida "results"]
    
    ;; Preparar entorno
    (asegurar-carpeta carpeta-salida)
    
    ;; Cargar configuración
    (let [config (leer-configuracion ruta-options)]
      (println (str "Configuración cargada: " 
                   (name (:measures config)) " | "
                   (name (:temp config)) " | "
                   (:servings config) " porciones | "
                   "filtro: " (:recipe_type config)))
      
      ;; Cargar y procesar recetas
      (let [recetas-originales (leer-archivos-de-receta ruta-recetas)]
        
        (if (empty? recetas-originales)
          (println "ERROR: No se encontraron recetas para procesar")
          (do
            (println (str "Recetas encontradas: " (count recetas-originales)))
            
            ;; Pipeline de procesamiento
            (let [recetas-filtradas (filtrar-recetas recetas-originales (:recipe_type config))
                  _ (println (str "Recetas después del filtro: " (count recetas-filtradas)))
                  recetas-limpias (mapv #(update % :ingredients limpiar-ingredientes) recetas-filtradas)
                  _ (println "Iniciando procesamiento paralelo...")
                  recetas-transformadas (procesar-recetas-paralelo recetas-limpias config)
                  _ (println "Generando archivos HTML...")]
              
              ;; Generar salidas
              (generar-html-para-recetas recetas-transformadas)
              
              ;; Estadísticas finales
              (imprimir-estadisticas config recetas-originales recetas-transformadas)
              
              ;; Retornar para testing
              recetas-transformadas)))))
    
    (println "=== Procesamiento completado exitosamente ===")))

;; Ejecutar la función principal si este archivo es ejecutado directamente
(when (= *ns* 'main-noparalel)
  (-main))