(ns mainAdd commentMore actions
    (:require [clojure.java.io :as io]
              [clojure.string :as str]))

; --función para convertir milisegundos a segundos--
(defn ms->seconds [ms] (/ ms 1000.0))

; --función para leer líneas de un archivo--
(defn read-lines [file-route]
  (clojure.string/split-lines (slurp file-route)))

; --función que regresa la lista de archivos dentro de la carpeta--
(defn folder-files [route]
    (->> (file-seq (io/file route))
        (filter #(.isFile %))
    ))

; --función que lee todos los archivos en una carpeta y los guarda en un lista no lazy[]--
(defn read-all-files [route]
    (let [files (folder-files route)
        tareas ; contiene la colección de futures
            (mapv #(future (read-lines (.getPath %))) files)] ; Para cada archivo, se crea una tarea asíncrona con future
        (mapv deref tareas) ; mapv regresa [] en lugar de () con map
    ))



(def start-time (System/currentTimeMillis))
    (def recipes_lst(read-all-files "recipe_collection/"))
(def end-time (System/currentTimeMillis))
(println "Tiempo de ejecución lectura de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

;(println "Tiempo de ejecución análisis de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

;(println "Tiempo de ejecución filtrado de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

;(println "Tiempo de ejecución transformación de archivos paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")

;(println "Tiempo de ejecución creación de htmls paralelo" (ms->seconds (- end-time start-time)) "s =" (- end-time start-time) "ms")