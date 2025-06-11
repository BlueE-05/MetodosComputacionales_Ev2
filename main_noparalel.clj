(ns main-noparalel
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [parsers :as parsers]
            [parsers-input :as parsers-input]
            [convert :as convert]
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
    
    (convert/convert-recipe
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

;; Función para medir tiempo de ejecución
(defn medir-tiempo [descripcion f]
  (let [inicio (System/currentTimeMillis)
        resultado (f)
        fin (System/currentTimeMillis)
        tiempo-ms (- fin inicio)]
    (println (str descripcion ": " tiempo-ms " ms"))
    {:resultado resultado :tiempo-ms tiempo-ms}))

;; Función principal del sistema
(defn -main []
  ;; Mensaje inicial
  (println "=== SISTEMA DE PROCESAMIENTO DE RECETAS (SECUENCIAL) ===")
  
  ;; INICIO DEL TIEMPO TOTAL
  (let [inicio-total (System/currentTimeMillis)]
    
    ;; Reinicia el contador global de recetas HTML generadas
    (reset! create-html/recipe-counter 0)
    
    ;; Define rutas de archivos y carpetas principales
    (let [ruta-options "input_testing/options1.txt"
          ruta-recetas "recipe_collection"
          carpeta-salida "results"]
      
      ;; Asegura que la carpeta de resultados exista
      (asegurar-carpeta carpeta-salida)
      
      ;; TIEMPO 1: Lectura de configuración desde archivo de opciones
      (let [{config :resultado tiempo-config :tiempo-ms}
            (medir-tiempo "Tiempo de lectura de configuración"
                         #(leer-configuracion ruta-options))]
        
        ;; Muestra la configuración cargada
        (println (str "Configuración: " (name (:measures config)) 
                     " | " (name (:temp config)) 
                     " | " (:servings config) " porciones"
                     " | filtro: " (:recipe_type config)))
        
        ;; TIEMPO 2: Lectura de archivos de recetas desde la carpeta
        (let [{recetas-originales :resultado tiempo-lectura :tiempo-ms}
              (medir-tiempo "Tiempo de lectura de archivos de recetas"
                           #(leer-archivos-de-receta ruta-recetas))]
          
          ;; Si no hay recetas, muestra error y termina
          (if (empty? recetas-originales)
            (println "ERROR: No se encontraron recetas")
            (do
              ;; Muestra cuántas recetas se encontraron
              (println (str "Recetas encontradas: " (count recetas-originales)))
              
              ;; TIEMPO 3: Filtrado de recetas por tipo/categoría
              (let [{recetas-filtradas :resultado tiempo-filtrado :tiempo-ms}
                    (medir-tiempo "Tiempo de filtrado de recetas"
                                 #(filtrar-recetas recetas-originales (:recipe_type config)))]
                
                ;; Muestra cuántas recetas quedan tras el filtro
                (println (str "Recetas después del filtro '" (:recipe_type config) "': " 
                             (count recetas-filtradas)))
                
                ;; Si no hay recetas tras el filtro, muestra mensaje y termina
                (if (empty? recetas-filtradas)
                  (println "No hay recetas que coincidan con el filtro")
                  (do
                    ;; TIEMPO 4: Creación del archivo de estilos CSS
                    (let [{_ :resultado tiempo-estilos :tiempo-ms}
                          (medir-tiempo "Tiempo de creación de estilos"
                                       #(create-styles/create-styles-file))]
                      
                      ;; TIEMPO 5: Procesamiento principal de recetas
                      (let [{_ :resultado tiempo-procesamiento :tiempo-ms}
                            (medir-tiempo "Tiempo de procesamiento de recetas"
                                         (fn []
                                           (println "Procesando recetas...")
                                           ;; Procesa cada receta filtrada:
                                           (doseq [receta recetas-filtradas]
                                             (let [titulo (get-in receta [:tokens :title] "Sin título")
                                                   receta-limpia (update receta :ingredients limpiar-ingredientes)
                                                   receta-procesada (procesar-receta receta-limpia config)]
                                               ;; Muestra el título de la receta que se está procesando
                                               (println (str "  - Procesando: " titulo))
                                               ;; Genera el archivo HTML para la receta procesada
                                               (create-html/create-html-file receta-procesada)))))]
                        
                        ;; Espera breve para asegurar escritura de archivos
                        (Thread/sleep 500)
                        
                        ;; TIEMPO 6: Verificación de archivos HTML generados
                        (let [{files-count :resultado tiempo-verificacion :tiempo-ms}
                              (medir-tiempo "Tiempo de verificación de archivos"
                                           #(create-html/verify-files))]
                          
                          ;; Muestra el resultado final de archivos generados
                          (println (str "RESULTADO FINAL: " files-count " archivos HTML generados exitosamente"))
                          
                          ;; Calcula y muestra el tiempo total de ejecución y resumen de tiempos
                          (let [fin-total (System/currentTimeMillis)
                                tiempo-total (- fin-total inicio-total)]
                            
                            (println "\n=== RESUMEN DE TIEMPOS (SECUENCIAL) ===")
                            (println (str "1. Lectura de configuración: " tiempo-config " ms"))
                            (println (str "2. Lectura de archivos: " tiempo-lectura " ms"))
                            (println (str "3. Filtrado de recetas: " tiempo-filtrado " ms"))
                            (println (str "4. Creación de estilos: " tiempo-estilos " ms"))
                            (println (str "5. Procesamiento principal: " tiempo-procesamiento " ms"))
                            (println (str "6. Verificación de archivos: " tiempo-verificacion " ms"))
                            (println (str "TIEMPO TOTAL: " tiempo-total " ms"))
                            (println (str "TIEMPO TOTAL: " (/ tiempo-total 1000.0) " segundos"))
                            (println "Procesamiento completado exitosamente")
                            (println "Tiempos mostrados en consola")))))))))))))
  
  ;; Mensaje final
  (println "=== FIN DEL PROCESAMIENTO ===")))

;; Ejecutar la función principal si este archivo es ejecutado directamente
(when (= *ns* 'main-noparalel)
  (-main))