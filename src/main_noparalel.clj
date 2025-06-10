(ns main-noparalel
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [parsers :as parsers]
            [parsers_input :as parsers-input]
            [conversions :as conversions]
            [create-html :as create-html]
            [create-styles :as create-styles]))

;; -------------------------------
;; PASO 1: Leer configuración y recetas
;; -------------------------------

(defn leer-configuracion [ruta-options]
  (let [config (-> ruta-options
                   slurp
                   str/split-lines
                   parsers_input/parser-input)
        config (into {} (map (fn [[k v]] [(keyword k) v]) config))]
    (println "Config leída:" config)
    (-> config
        (update :measures #(when % (keyword %)))
        (update :temp #(when % (keyword %)))
        (update :servings #(cond
                             (string? %) (Integer/parseInt %)
                             (number? %) %
                             :else nil))
        (update :recipe_type #(when % (str/lower-case %))))))

(defn leer-archivos-de-receta [ruta-carpeta]
  (->> (file-seq (io/file ruta-carpeta))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".txt")))
       (map #(-> % slurp str/split-lines parsers/build-recipe))
       vec))

;; -------------------------------
;; PASO 2: Filtrar recetas por categoría
;; -------------------------------

(defn filtrar-recetas [recetas filtro]
  (if (= filtro "all")
    recetas
    (filter #(= (name (:category %)) filtro) recetas)))

;; -------------------------------
;; PASO 3: Transformar recetas (escalar, unidades, temp, calorías)
;; -------------------------------

(defn transformar-recetas [recetas config]
  (let [sistema (when (:measures config) (name (:measures config)))
        nueva-temp (when (:temp config) (name (:temp config)))
        porciones-nuevas (:servings config)]
    (mapv (fn [receta]
            (let [porciones-actuales (:servings receta)]
              (conversions/convert-recipe
                receta
                {:sistema sistema
                 :porciones-actuales porciones-actuales
                 :porciones-nuevas porciones-nuevas
                 :temperature-unit nueva-temp})))
          recetas)))

;; -------------------------------
;; PASO 4: Generar HTML
;; -------------------------------

(defn generar-html-para-recetas [recetas]
  (doseq [receta recetas]
    (create-html/create-html-file receta)))

;; -------------------------------
;; Función principal (ejecuta todo)
;; -------------------------------

(defn limpiar-ingredientes [ingredientes]
  (filter #(and (some? (:type %)) (some? (:quantity %))) ingredientes))

(defn asegurar-carpeta [ruta]
  (let [dir (io/file ruta)]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn -main []
  (let [ruta-options "input_testing/options1.txt"
        ruta-recetas "recipe_collection"
        carpeta-salida "results"]
    (asegurar-carpeta carpeta-salida)
    (create-styles/create-styles-file)
    (let [config (leer-configuracion ruta-options)
          recetas (leer-archivos-de-receta ruta-recetas)]
      (println "Configuración completa:" config)
      (println "Recetas leídas:" (count recetas) recetas)
      (println "Categorias de recetas leídas:" (map :category recetas))
      ;; (let [recetas-filtradas (filtrar-recetas recetas (:recipe_type config))
      ;;       recetas-limpias (map #(update % :ingredients limpiar-ingredientes) recetas-filtradas)
      ;;       recetas-transformadas (transformar-recetas recetas-limpias config)]
      ;;   (println "Recetas transformadas:" (count recetas-transformadas) recetas-transformadas)
      ;;   (generar-html-para-recetas recetas-transformadas)
      ;;   (println (str "✅ Se generaron " (count recetas-transformadas) " recetas en 'results/'")))
      ;; )
      ;; En vez de filtrar/limpiar/transformar, solo genera HTML con las recetas originales:
      (generar-html-para-recetas recetas)
      (println (str "✅ Se generaron " (count recetas) " recetas en 'results/'"))
      )))

;; Ejecutar la función principal si este archivo es ejecutado directamente
(when (= *ns* 'main-noparalel)
  (-main))

