(ns create-html
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [create-styles :as create-styles]))

;; Contador global para numeración de archivos HTML
(def recipe-counter (atom 0))

;; HTML templates constantes para reducir repetición
(def html-head
  ["<!DOCTYPE html>"
   "<html lang=\"en\">"
   "<head>"
   "\t<link href=\"https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible&display=swap\" rel=\"stylesheet\">"
   "\t<meta charset=\"UTF-8\">"
   "\t<link rel=\"icon\" href=\"https://cdn-icons-png.flaticon.com/512/3183/3183463.png\" type=\"image/png\">"
   "\t<link rel=\"stylesheet\" href=\"style.css\">"])

(def navbar-html
  ["\t<nav class=\"navbar\">"
   "\t\t<ul class=\"navbar-content\">"
   "\t\t\t<li class=\"logo-group\">"
   "\t\t\t\t<img src=\"https://cdn-icons-png.flaticon.com/512/3183/3183463.png\" alt=\"Logo\" class=\"logo\">"
   "\t\t\t\t<span class=\"logoText\">SuperKitchen</span>"
   "\t\t\t</li>"
   "\t\t\t<li>Recipe of the Day</li>"
   "\t\t\t<li>Recipes</li>"
   "\t\t\t<li>About Us</li>"
   "\t\t\t<li>"
   "\t\t\t\t<svg class=\"login-icon\" width=\"25\" height=\"25\" viewBox=\"0 0 24 24\"><path fill=\"#ffffff\" fill-rule=\"evenodd\" d=\"M12 1C5.925 1 1 5.925 1 12c0 2.585.892 4.962 2.384 6.84l.086-.344c.006-.024.01-.043.017-.084l.017-.095c.017-.092.047-.245.108-.41.201-.532.647-.998 1.112-1.371.493-.396 1.122-.777 1.846-1.11A13.177 13.177 0 0 1 12 14.25c2.084 0 3.983.508 5.43 1.175.724.334 1.353.715 1.846 1.10.465.374.91.84 1.111 1.373.062.164.092.317.11.409l.016.095c.007.041.01.06.017.084l.086.344A10.953 10.953 0 0 0 23 12c0-6.075-4.925-11-11-11zm0 3.5a4.75 4.75 0 1 0 0 9.5 4.75 4.75 0 0 0 0-9.5z\" clip-rule=\"evenodd\" /></svg>"
   "\t\t\t\tLog In"
   "\t\t\t</li>"
   "\t\t</ul>"
   "\t</nav>"])

(def html-footer ["\t</main>" "</body>" "</html>"])

;; Convierte lista de líneas a string con saltos de línea
(defn add-endl [lines] (str/join "\n" lines))

;; Formatea cantidades numéricas para mostrar en recetas
(defn format-quantity [qty]
  (cond
    (nil? qty) ""
    (number? qty)
    (let [qty-double (double qty)
          rounded (if (< qty-double 1)
                    (/ (Math/round (* qty-double 8.0)) 8.0)
                    (/ (Math/round (* qty-double 100.0)) 100.0))]
      (if (= rounded (Math/floor (double rounded)))
        (str (int rounded))
        (str rounded)))
    :else (str qty)))

;; Limpia valores de tiempo removiendo duplicaciones
(defn clean-time-value [time-str]
  (when time-str
    (-> (str time-str)
        (str/replace #"(\d+)\s*minutes?\s*mins?\s*minutes?" "$1")
        (str/replace #"(\d+)\s*mins?\s*minutes?" "$1") 
        (str/replace #"(\d+)\s*minutes?\s*mins?" "$1")
        (str/replace #"\s+minutes\s+minutes|mins\s+mins" "")
        str/trim)))

;; Extrae información de equipment de la descripción usando patrones genéricos
(defn extract-equipment [description]
  (when description
    (let [equipment-match (some #(re-find % description)
                                [#"Equipment:?\s*(-[^.]*?)(?=\s*$|\.|[A-Z][a-z]+:)"
                                 #"Equipment:?\s*([^.]*?)(?=\s*$|\.|[A-Z][a-z]+:)"
                                 #"Equipment:?\s*(.+?)(?=\s*$|\.)"
                                 #"([A-Z][a-z]+\s+and\s+[A-Z][a-z]+)\s*$"
                                 #"(-[A-Z0-9][^-]*-[A-Z][^-]*-[A-Z][^-]*)\s*$"])]
      (when equipment-match
        (let [equipment-text (second equipment-match)]
          (cond
            (str/includes? equipment-text "-")
            (->> (str/split equipment-text #"-")
                 (map str/trim)
                 (filter #(not (str/blank? %)))
                 (map #(str/replace % #"^-\s*" "")))
            
            (str/includes? equipment-text " and ")
            (->> (str/split equipment-text #"\s+and\s+")
                 (map str/trim)
                 (filter #(not (str/blank? %))))
            
            (str/includes? equipment-text ",")
            (->> (str/split equipment-text #",")
                 (map str/trim)
                 (filter #(not (str/blank? %))))
            
            :else [(str/trim equipment-text)]))))))

;; Limpia descripciones removiendo equipment y metadata usando filtrado por oraciones
(defn clean-description [description]
  (when (and description (not (str/blank? description)))
    (let [sentences (str/split description #"\.\s+")
          clean-sentences (filter (fn [sentence]
                                    (not (some #(re-find % sentence)
                                               [#"Equipment:?"
                                                #"^-[A-Z0-9].*-[A-Z].*-[A-Z]"
                                                #"^[A-Z][a-z]+\s+and\s+[A-Z][a-z]+$"
                                                #"Prep Time:|Cook Time:|Total Time:"
                                                #"Serves?\s*-?\s*\d+"
                                                #"Category:"
                                                #"^From\s+.*by\s+"
                                                #"^Submitted by\s+"])))
                                  sentences)]
      (when (seq clean-sentences)
        (-> (str/join ". " clean-sentences)
            (str/replace #"\.{2,}" ".")
            (str/replace #"\s{2,}" " ")
            str/trim
            (#(if (and (not (str/blank? %)) (not (str/ends-with? % ".")))
                (str % ".")
                %)))))))

;; Extrae información de autor de la descripción usando patrones robustos
(defn extract-author-from-description [description]
  (when description
    (some #(when-let [match (re-find % description)]
             (case (count match)
               3 (str "From " (str/trim (nth match 1)) " by " (str/trim (nth match 2)))
               2 (case (first (str/split (first match) #"\s+"))
                   "Submitted" (str "Submitted by " (str/trim (second match)))
                   "From" (str "From " (str/trim (second match)))
                   (str "By " (str/trim (second match)) " of " (str/trim (nth match 2))))))
          [#"From\s+([^.]+?)\s+by\s+([^.]+?)(?:\.|$)"
           #"Submitted by\s+([A-Z]+)(?:\s|$)"
           #"By\s+([^.]+?)\s+of\s+([^.]+?)(?:\.|$)"
           #"From\s+([^.]+?)(?:\.|$)"])))

;; Crea párrafos HTML para título, descripción y autor
(defn create-content-paragraphs [tokens-map]
  (let [title (:title tokens-map)
        original-author (:author tokens-map)
        description (:description tokens-map)
        extracted-author (when (and (not original-author) description)
                          (extract-author-from-description description))
        final-author (or original-author extracted-author)
        cleaned-desc (clean-description description)]
    
    (filter some?
      [(when title (format "\t<h1 class=\"title\">%s</h1>" title))
       (when (and cleaned-desc (not (str/blank? cleaned-desc)) (> (count cleaned-desc) 10))
         (format "\t<p class=\"description\">%s</p>" cleaned-desc))
       (when final-author (format "\t<p class=\"author\">%s</p>" final-author))])))

;; Crea sección HTML genérica con formato estándar
(defn create-section [section-type title items format-fn]
  (concat
    [(format "\t\t<section class=\"%s\">" section-type)
     (format "\t\t\t<%s>%s</%s>" 
             (if (= section-type "equipment") "h3" "h2") 
             title 
             (if (= section-type "equipment") "h3" "h2"))]
    (map format-fn items)
    ["\t\t</section>"]))

;; Formateadores específicos para cada tipo de contenido
(defn format-meta-item [[label value unit]]
  (let [unit-str (if unit (str " " unit) "")]
    (format "\t\t\t<div class=\"meta-item\"><strong>%s:</strong> %s%s</div>" 
            label value unit-str)))

(defn format-equipment-item [item]
  (format "\t\t\t<div class=\"equipment-item\">%s</div>" 
          (-> item (str/replace #"^-\s*" "") str/trim)))

(defn format-ingredient-item [ingred]
  (let [qty (format-quantity (:quantity ingred))
        unit (when (:unit ingred) (str (name (:unit ingred)) " "))
        text (:text ingred)
        ingredient-str (str (when (not (str/blank? qty)) (str qty " "))
                            (when unit unit) text)]
    (format "\t\t\t<div class=\"ingredient\"><strong>%s</strong></div>" (str/trim ingredient-str))))

(defn format-instruction-item [instr]
  (format "\t\t\t<div class=\"instruction\"><strong>%s.</strong> <strong>%s</strong></div>"
          (:step instr) (:text instr)))

;; Genera todas las secciones de contenido
(defn create-all-sections [recipe]
  (let [metadata (:metadata recipe)
        category (when (:category recipe) (name (:category recipe)))
        servings (:servings recipe)
        tokens (:tokens recipe)
        
        meta-fields [["Category" category]
                     ["Prep Time" (clean-time-value (:prep-time metadata)) "minutes"]
                     ["Cook Time" (clean-time-value (:cook-time metadata)) "minutes"] 
                     ["Total Time" (clean-time-value (:total-time metadata)) "minutes"]
                     ["Servings" servings]
                     ["Total Calories" (:calories metadata) "kcal"]
                     ["Calories Per Serving" (:calories-per-serving metadata) "kcal"]]
        
        valid-meta-fields (filter #(some? (second %)) meta-fields)
        equipment-items (when-let [desc (:description tokens)]
                         (extract-equipment desc))]
    
    (concat
      (create-section "meta" "" valid-meta-fields format-meta-item)
      (when (seq equipment-items)
        (create-section "equipment" "Equipment" equipment-items format-equipment-item))
      (create-section "ingredients" "Ingredients" (:ingredients recipe) format-ingredient-item)
      (create-section "instructions" "Instructions" (:instructions recipe) format-instruction-item))))

;; Genera el contenido HTML completo de la receta
(defn html-content [recipe]
  (try
    (let [title (:title (:tokens recipe))
          content-paragraphs (create-content-paragraphs (:tokens recipe))]
      (concat
        html-head
        [(str "\t<title>" title "</title>")
         "</head>"
         "<body>"]
        navbar-html
        ["\t<main class=\"content\">"]
        content-paragraphs
        (create-all-sections recipe)
        html-footer))
    (catch Exception e [])))

;; Asegura que el directorio results existe
(defn ensure-results-directory []
  (try
    (let [results-dir (io/file "results")]
      (when-not (.exists results-dir) (.mkdirs results-dir))
      true)
    (catch Exception e false)))

;; Crea un archivo HTML individual para una receta
(defn create-html-file [recipe]
  (try
    (ensure-results-directory)
    (let [html-lines (html-content recipe)]
      (when (seq html-lines)
        (let [new-num (swap! recipe-counter inc)
              filename (str "results/recipe" new-num ".html")]
          (spit filename (add-endl html-lines))
          (Thread/sleep 50)
          (.exists (io/file filename)))))
    (catch Exception e false)))

;; Verifica y cuenta archivos HTML generados
(defn verify-files []
  (try
    (let [results-dir (io/file "results")]
      (when (.exists results-dir)
        (count (filter #(str/ends-with? (.getName %) ".html") 
                       (filter #(.isFile %) (.listFiles results-dir))))))
    (catch Exception e 0)))