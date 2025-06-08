(ns main-noparalel)

; --función que --
(defn leer-coleccion-recetas [ruta-directorio]
  (mapv #(leer-lineas (.getPath %)) (archivos-en-directorio ruta-directorio))) ; mapv regresa [] en lugar de () con map