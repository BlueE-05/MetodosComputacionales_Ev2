(ns paralelo)

;; Carga los archivos fuente con funciones auxiliares y módulos de procesamiento
(load-file "src/parsers_input.clj")   ; Parser para el archivo de opciones/configuración
(load-file "src/parsers.clj")         ; Parser para las líneas de recetas
(load-file "src/convert.clj")         ; Funciones de conversión de unidades y porciones
(load-file "src/create_styles.clj")   ; Generador de archivo CSS
(load-file "src/create_html.clj")     ; Generador de archivos HTML

;; Importa librerías estándar de Clojure
(require '[clojure.java.io :as io])   ; Para manejo de archivos y carpetas
(require '[clojure.string :as str])   ; Para manipulación de strings

;;* Utils

; --función para convertir milisegundos a segundos--
(defn ms->seconds [ms]
  "Convierte milisegundos a segundos (float)."
  (/ ms 1000.0))

;; THREADS (intento de optimizar threads :pp)

; --función que obtiene núcleos disponibles en el sistema--
(defn available-cores []
  "Devuelve el número de núcleos de CPU disponibles."
  (.availableProcessors (Runtime/getRuntime)))

; --función que determina el número de threads ideales en la compu--
(defn ideal-n-threads [task-type]
    (let [cores (available-cores)]
        (case task-type
        ; cpu = procesos, mate
        ; io = leer archivos
        :cpu cores
        :io (* cores 4) ; Cuando una tarea es I/O-bound, no está usando el CPU todo el tiempo.
                        ; Durante ese tiempo de espera, el hilo está inactivo (bloqueado), y el CPU podría estar haciendo otra cosa
                        ; Con cores*4 espera maso 75% del tiempo
        cores)))

; --función que calcula el chunk size para repartir equitativamente los datos (datos/threads + 1)--
(defn calc-thread-chunks [data-lst task-type]
  "Calcula el número de threads y el tamaño de chunk para paralelizar una lista de datos."
  (let [n-threads (ideal-n-threads task-type)
        total (count data-lst)
        chunk-size (int (Math/ceil (/ (double total) n-threads)))]
    {:threads n-threads
     :chunk-size chunk-size}))

;;* Proceso

; --función para leer líneas de un archivo--
(defn read-lines [file-route]
  "Lee todas las líneas de un archivo de texto y las regresa como lista de strings."
  (str/split-lines (slurp file-route)))

; --función que regresa la lista de archivos dentro de la carpeta--
(defn folder-files [route]
  "Devuelve una secuencia de archivos (no carpetas) dentro de una ruta dada."
  (->> (file-seq (io/file route))
       (filter #(.isFile %))))

(defn read-all-files [route] ; si parece ser más rápida al usar n-threads y chunk-size
  (let [files (vec (folder-files route))
        {:keys [threads chunk-size]} (calc-thread-chunks files :io)
        ; Particionamos la lista de archivos en chunks
        file-chunks (partition-all chunk-size files)
        tareas
          (mapv
            #(future
               (mapv (fn [f] (read-lines (.getPath f))) %))
            file-chunks)]
    ; Aplanamos la lista de resultados: [[...][...]] → [...]
    (vec (apply concat (mapv deref tareas)))))

; --parser para cada lineas--
(defn parser-all-lines [lines-lst]
  "Paraleliza el parseo de líneas de texto a recetas usando chunks."
  (let [{:keys [threads chunk-size]} (calc-thread-chunks lines-lst :cpu) ; Determina chunks y threads
        line-chunks (partition-all chunk-size lines-lst)
        tareas (mapv
                 (fn [chunk] (future (mapv parsers/build-recipe chunk))) ; Parsea cada chunk en un thread
                 line-chunks)]
    (vec (apply concat (mapv deref tareas))))) ; Junta todos los resultados

; --filtrar para solo transformar los archivos necesarios --
(defn filter-recipes [recipe-lst desired_category]
  "Filtra recetas por categoría en paralelo. Si desired_category es 'all', no filtra nada."
  (if (= (str/lower-case desired_category) "all")
    recipe-lst
    (let [desired (str/lower-case desired_category)
          {:keys [threads chunk-size]} (calc-thread-chunks recipe-lst :cpu)
          recipe-chunks (partition-all chunk-size recipe-lst)]
      
      (println (str "Filtrando " (count recipe-lst) " recetas por categoría '" desired "' con " threads " threads..."))
      
      (->> recipe-chunks
           (map (fn [chunk]
                  (future
                    (doall
                      (filter #(= (str/lower-case (name (:category %))) desired) chunk)))))
           (mapv deref)
           (apply concat)
           vec))))

; -- Helper function to ensure recipe has required fields --
(defn ensure-recipe-structure [recipe]
  "Asegura que la receta tenga los campos esenciales con valores por defecto."
  (-> recipe
      (update :ingredients #(or % []))
      (update :instructions #(or % []))
      (assoc :measure-system (or (:measure-system recipe) :metric))
      (assoc :temperature-unit (or (:temperature-unit recipe) :C))
      (assoc :servings (or (:servings recipe) 4))))

; --función para limpiar ingredientes inválidos--
(defn limpiar-ingredientes [ingredientes]
  "Filtra ingredientes inválidos (sin cantidad o texto)."
  (filter #(and (some? (:quantity %))
                (some? (:text %))
                (not (str/blank? (str (:text %)))))
          ingredientes))

; --función mejorada para procesar una sola receta--
(defn procesar-receta [receta config]
  "Procesa una receta aplicando conversiones de sistema, temperatura y porciones."
  (try
    (let [sistema (or (get config "sistema") (get config "measures")) ; Obtiene sistema de medidas deseado
          nueva-temp (get config "temp") ; Obtiene unidad de temperatura deseada
          porciones-nuevas (let [p (or (get config "porciones") (get config "servings"))]
                             (if (string? p) (Integer/parseInt p) p)) ; Convierte porciones a int si es string
          porciones-actuales (:servings receta) ; Porciones actuales de la receta
          receta-limpia (update receta :ingredients limpiar-ingredientes)] ; Limpia ingredientes inválidos
      ;; Aplica la conversión usando la configuración leída
      (convert/convert-recipe
        receta-limpia
        {:sistema sistema
         :porciones-actuales porciones-actuales
         :porciones-nuevas porciones-nuevas
         :temperature-unit nueva-temp}))
    (catch Exception e
      (println (str "Error procesando receta " (get-in receta [:tokens :title] "sin título") ": " (.getMessage e)))
      receta))) ; Devuelve la receta original si hay error

; -- Procesar todas las recetas --
(defn process-all-recipes-parallel [recipes-lst config]
  "Procesa todas las recetas aplicando todas las transformaciones necesarias"
  (let [{:keys [threads chunk-size]} (calc-thread-chunks recipes-lst :cpu) ; Calcula el número de threads y tamaño de chunk para procesamiento paralelo
        recipe-chunks (partition-all chunk-size recipes-lst)] ; Divide la lista de recetas en chunks para cada thread
    
    (println (str "Procesando TODAS las transformaciones de " (count recipes-lst) " recetas con " threads " threads..."))
    
    (->> recipe-chunks
         (map (fn [chunk]
                (future
                  (doall
                    (map (fn [recipe]
                           ;; Procesa cada receta individualmente aplicando las transformaciones (porciones, sistema, temperatura)
                           (procesar-receta recipe config))
                         chunk))))) ; Cada chunk se procesa en un thread (future)
         (mapv deref) ; Espera a que todos los threads terminen y recoge los resultados
         (apply concat) ; Junta todos los resultados de los chunks en una sola lista
         vec))) ; Devuelve un vector con todas las recetas procesadas

; --creación con threads de todos los html--
(defn create-html-files-parallel [recipe_lst]
  (let [{:keys [threads chunk-size]} (calc-thread-chunks recipe_lst :cpu)
        recipe-chunks (partition-all chunk-size recipe_lst) ; chunks para cada hilo
        ; un future por chunk que crea cada html y devuelve count éxitos
        tareas
         (mapv
           (fn [chunk]
             (future
               ; crear todos y contar los archivos generados
               (reduce
                 (fn [cnt recipe]
                   (if (create-html/create-html-file recipe)
                     (inc cnt)
                     cnt))
                 0 ;empieza en 0
                 chunk)))
           recipe-chunks)]
    (reduce + (mapv deref tareas)) ; deref para espera y luego sumar los resultados
))

;;* Main F
(defn -main []
  ;; Paso 1: Definir la ruta del archivo de configuración y leer las opciones del usuario
  (let [config-file "input_testing/options1.txt" 
        options (parsers-input/parser-input (read-lines config-file))] ; Lee y parsea el archivo de configuración
    (println (str "Configuración cargada desde " config-file ": " options))
    
    ;; Paso 2: Leer todos los archivos de recetas y medir el tiempo que toma
    (let [start-time (System/currentTimeMillis)
          lines_lst (read-all-files "recipe_collection/") ; Lee todas las líneas de todos los archivos de recetas
          end-time (System/currentTimeMillis)]
      (println "Tiempo de ejecución lectura de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
      
      ;; Paso 3: Parsear las líneas leídas a estructuras de recetas y filtrar por categoría
      (let [start-time (System/currentTimeMillis)
            recipes_lst (parser-all-lines lines_lst) ; Parsea todas las líneas a recetas
            end-time (System/currentTimeMillis)
            recipes_lst (filter-recipes recipes_lst (or (get options "filtra") (get options "recipe_type")))] ; Filtra recetas por categoría
        (println "Tiempo de ejecución análisis de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
        (println (str "Recetas filtradas: " (count recipes_lst)))
        
        ;; Paso 4: Procesar todas las recetas aplicando las transformaciones necesarias en paralelo (porciones, sistema, temperatura)
        (let [start-time (System/currentTimeMillis)
              recipes_lst (process-all-recipes-parallel recipes_lst options)
              end-time (System/currentTimeMillis)]
          (println "Tiempo de ejecución transformación de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
          (println "Primera receta procesada:" (get-in (first recipes_lst) [:tokens :title] "Sin título"))
          
          ;; Paso 5: Crear el archivo de estilos CSS para los HTMLs generados
          (let [start-time (System/currentTimeMillis)
                css-task (future (create-styles/create-styles-file))
                _ @css-task ; Espera a que termine
                end-time (System/currentTimeMillis)]
            (println "Tiempo de creación de CSS paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms"))
          
          ;; Paso 6: Crear los archivos HTML de las recetas en paralelo y medir el tiempo
          (let [start-time (System/currentTimeMillis)
                created-count (create-html-files-parallel recipes_lst)
                end-time (System/currentTimeMillis)]
            (println "Tiempo de ejecución creación de htmls paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
            (println (str "RESULTADO FINAL: " created-count " archivos HTML generados exitosamente"))))))))

;; Ejecuta la función principal
(when (= *ns* 'paralelo)
  (-main))