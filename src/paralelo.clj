(load-file "parsers_input.clj")
(load-file "parsers.clj")
(load-file "convert.clj")
(load-file "create_styles.clj") ; el orden de los imports si importa
(load-file "create_html.clj")

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;* Utils
; --función para convertir milisegundos a segundos--
(defn ms->seconds [ms] (/ ms 1000.0))

;; THREADS (intento de optimizar threads :pp)
; --función que obtiene núcleos disponibles en el sistema--
(defn available-cores []
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
  (let [n-threads (ideal-n-threads task-type)
        total (count data-lst)
        chunk-size (int (Math/ceil (/ (double total) n-threads)))]
    {:threads n-threads
     :chunk-size chunk-size}))

;;* Proceso
; --función para leer líneas de un archivo--
(defn read-lines [file-route]
  (str/split-lines (slurp file-route)))

; --función que regresa la lista de archivos dentro de la carpeta--
(defn folder-files [route]
    (->> (file-seq (io/file route))
        (filter #(.isFile %))
    ))

; --función que lee todos los archivos en una carpeta y los guarda en un lista no lazy[]--
(defn read-all-files-og [route]
    (let [files (folder-files route)
        tareas ; contiene la colección de futures
            (mapv #(future (read-lines (.getPath %))) files)] ; Para cada archivo, se crea una tarea asíncrona con future
        (mapv deref tareas) ; mapv regresa [] en lugar de () con map
    ))

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
  (let [
        {:keys [threads chunk-size]} (calc-thread-chunks lines-lst :cpu) ; determinamos chunks y threads para procesamiento paralelo
        line-chunks (partition-all chunk-size lines-lst)
        tareas (mapv ; []
                 (fn [chunk] (future (mapv parsers/build-recipe chunk)))
                 line-chunks)]
    (vec (apply concat (mapv deref tareas)))
    ))

; --filtrar para solo transformar los archivos necesarios--
(defn filter-recipes [recipe-lst desired_category]
  (if (= (str/lower-case desired_category) "all")
    recipe-lst
    (let [desired (str/lower-case desired_category)]
      (filter #(= (str/lower-case (name (:category %))) desired) recipe-lst))))

; -- Helper function to ensure recipe has required fields --
(defn ensure-recipe-structure [recipe]
  (-> recipe
      (update :ingredients #(or % []))
      (update :instructions #(or % []))
      (assoc :measure-system (or (:measure-system recipe) :metric))
      (assoc :temperature-unit (or (:temperature-unit recipe) :C))
      (assoc :servings (or (:servings recipe) 4))))

; --convertir cantidad de servings para cada receta--
(defn transform-servings [recipes-lst desired-serves]
  (let [desired-serves (if (string? desired-serves) 
                         (Integer/parseInt desired-serves)
                         desired-serves)]
    (mapv
      (fn [recipe]
        (let [recipe (ensure-recipe-structure recipe)
              current-serves (:servings recipe)]
          (if (and current-serves (pos? current-serves))
            (convert/convert-recipe 
              recipe
              {:sistema (name (:measure-system recipe))
               :porciones-actuales current-serves
               :porciones-nuevas desired-serves
               :temperature-unit (name (:temperature-unit recipe))})
            recipe)))
      recipes-lst)))

; --convertir sistema de medición para cada receta--
(defn transform-system [recipes-lst desired-measures]
  (let [desired-measures (if (keyword? desired-measures)
                           desired-measures
                           (keyword desired-measures))]
    (mapv
      (fn [recipe]
        (let [recipe (ensure-recipe-structure recipe)
              current-system (:measure-system recipe)]
          (if (not= current-system desired-measures)
            (convert/convert-recipe
              recipe
              {:sistema (name desired-measures)
               :porciones-actuales (:servings recipe)
               :porciones-nuevas (:servings recipe)
               :temperature-unit (name (:temperature-unit recipe))})
            (assoc recipe :measure-system desired-measures))))
      recipes-lst)))

; --convertir temperaturas para cada receta--
(defn transform-temp [recipes-lst desired-temp]
  (let [desired-temp (if (keyword? desired-temp)
                       desired-temp
                       (keyword desired-temp))]
    (mapv
      (fn [recipe]
        (let [recipe (ensure-recipe-structure recipe)
              current-temp (:temperature-unit recipe)]
          (if (not= current-temp desired-temp)
            (convert/convert-recipe
              recipe
              {:sistema (name (:measure-system recipe))
               :porciones-actuales (:servings recipe)
               :porciones-nuevas (:servings recipe)
               :temperature-unit (name desired-temp)})
            (assoc recipe :temperature-unit desired-temp))))
      recipes-lst)))

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

;;* Main
(def options (parsers-input/parser-input (read-lines "../input_testing/options1.txt")))
(println options)
(def start-time (System/currentTimeMillis))
    (def lines_lst (read-all-files "../recipe_collection/"))
(def end-time (System/currentTimeMillis))
(println "Tiempo de ejecución lectura de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

(def start-time (System/currentTimeMillis))
    (def recipes_lst (parser-all-lines lines_lst))
(def end-time (System/currentTimeMillis))
(println "Tiempo de ejecución análisis de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

(def recipes_lst (filter-recipes recipes_lst (get options "recipe_type")))

(def start-time (System/currentTimeMillis))
    ;(def recipes_lst (transform-servings recipes_lst (get options "servings")))
    ;(def recipes_lst (transform-system recipes_lst (get options "measures")))
    ;(def recipes_lst (transform-temp recipes_lst (get options "temp")))
(def end-time (System/currentTimeMillis))
(println "Tiempo de ejecución transformación de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
(println (first recipes_lst))

(create-styles/create-styles-file)

(let [start-time (System/currentTimeMillis)
        created-count (create-html-files-parallel recipes_lst)
      end-time (System/currentTimeMillis)]
  ;(println "[debug] HTMLs generados:" created-count)
  (println "Tiempo de ejecución creación de htmls paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")
)