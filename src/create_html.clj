(ns create-html
  (:require [clojure.java.io :as io])  ; librería para manejar archivos y carpetas
  (:require [create-styles.clj])) ; funcion add-endl

;!TODO: cambiar a que reciba la nueva estructura de Recipe


; --función para obtener el número máximo de archivos 'recipe' en la carpeta 'results'--
(defn get-max-recipe-num [dir]
    (let [files (file-seq (io/file dir))
            recipe-files (filter #(re-matches #"recipe\d+\.html" (.getName %)) files)
            nums (map #(Integer/parseInt (second (re-find #"recipe(\d+)" (.getName %)))) recipe-files)]
        (if (empty? nums) 0 (apply max nums))
    ))

; --función para crear formato parrafo de html a partir de tokens--
(defn token->paragraph [[label value]]
  (cond
    (= label "title") (format "\t<p class=\"title\">%s</p>" value)
    (= label "description") (format "\t<p class=\"description\">%s</p>" value)
    (= label "author") (format "\t<p class=\"author\">%s</p>" value)
    :else (format "\t<p class=\"description\">%s</p>" value)))

; --función para crear el encabezado HTML a partir de una lista de tokens--
(defn create-html-header [token-lst]
  (let [title (->> token-lst
                   (filter #(= (first %) "title"))
                   first
                   second)
        paragraphs (map token->paragraph token-lst)]
    (concat
     ["<!DOCTYPE html>"
      "<html lang=\"en\">"
      "<head>"
      "\t<link href=\"https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible&display=swap\" rel=\"stylesheet\">"
      "\t<meta charset=\"UTF-8\">"
      "\t<link rel=\"icon\" href=\"https://cdn-icons-png.flaticon.com/512/3183/3183463.png\" type=\"image/png\">"
      (str "\t<title>" title "</title>")
      "\t<link rel=\"stylesheet\" href=\"style.css\">"
      "</head>"
      "<body>"
      "\t<nav class=\"navbar\">"
      "\t\t<ul class=\"navbar-content\">"
      "\t\t\t<li class=\"logo-group\">"
      "\t\t\t\t<img src=\"https://cdn-icons-png.flaticon.com/512/3183/3183463.png\" alt=\"Logo\" class=\"logo\">"
      "\t\t\t\t<span class=\"logoText\">SuperKitchen</span>"
      "\t\t\t</li>"
      "\t\t\t<li>Recipe of the Day</li>"
      "\t\t\t<li>Recipes</li>"
      "\t\t\t<li>About Us</li>"
      "\t\t\t<li>"
      "\t\t\t\t<svg class=\"login-icon\" width=\"25\" height=\"25\" viewBox=\"0 0 24 24\"><path fill=\"#ffffff\" fill-rule=\"evenodd\" d=\"M12 1C5.925 1 1 5.925 1 12c0 2.585.892 4.962 2.384 6.84l.086-.344c.006-.024.01-.043.017-.084l.017-.095c.017-.092.047-.245.108-.41.201-.532.647-.998 1.112-1.371.493-.396 1.122-.777 1.846-1.11A13.177 13.177 0 0 1 12 14.25c2.084 0 3.983.508 5.43 1.175.724.334 1.353.715 1.846 1.11.465.374.91.84 1.111 1.373.062.164.092.317.11.409l.016.095c.007.041.01.06.017.084l.086.344A10.953 10.953 0 0 0 23 12c0-6.075-4.925-11-11-11zm0 3.5a4.75 4.75 0 1 0 0 9.5 4.75 4.75 0 0 0 0-9.5z\" clip-rule=\"evenodd\" /></svg>"
      "\t\t\t\tLog In"
      "\t\t\t</li>"
      "\t\t</ul>"
      "\t</nav>"
      "\t<main class=\"content\">"]
     paragraphs
    )))

; --función para formatear los tokens de metadata a divs HTML--
(defn token->div [[label value]]
  (cond
    (= label "category") (format "\t\t\t<div><strong>Category:</strong>%s minutes</div>" value) ; al escribir aqui 'minutes' evito que se vea extraño en los casos donde minutes y mins vienen duplicados
    (= label "prep-time") (format "\t\t\t<div><strong>Prep Time:</strong>%s minutes</div>" value)
    (= label "cook-time") (format "\t\t\t<div><strong>Cook Time:</strong>%s minutes</div>" value)
    (= label "servings") (format "\t\t\t<div><strong>Servings:</strong>%s</div>" value)
    (= label "calories_serving") (format "\t\t\t<div><strong>Calories Per Serving:</strong>%s Kcal</div>" value)
    (= label "calories_total") (format "\t\t\t<div><strong>Total Calories:</strong>%s Kcal</div>" value)
    :else (format "\t<p class=\"description\">%s</p>" value)))
  ))

; --función para formatear las listas de ingredientes e instrucciones a divs HTML--
(defn lst->div [line] ; para instrucciones e ingredientes
  (format "\t\t\t<div>%s</div>" line))

; --función para crear la sección de metadata--
(defn create-html-meta [meta_lst] '(
  "\t\t<section class=\"meta\">"
  (map token->div meta_lst)
  "\t\t</section>"
  ))

; --funciones para crear la sección de ingredientes--
(defn create-html-ingredients [ingredients_lst]
  "\t\t<section class=\"ingredients\">"
  (map lst->div ingredients_lst)
  "\t\t</section>"
  ))

; --funciones para crear la sección de instrucciones--
(defn create-html-instructions [instructions_lst]
  "\t\t<section class=\"instructions\">"
  (map lst->div instructions_lst)
  "\t\t</section>"
  ))

; --función para crear el string completo del HTML a partir de los tokens--
(defn html-content [recipe]
  ; Obtener los tokens, metadata, ingredientes e instrucciones de la receta
  (let [token_lst (:tokens recipe)
        meta_lst (:metadata recipe)
        ingredients_lst (:ingredients recipe)
        instructions_lst (:instructions recipe)]
    [title (get-token token_lst "title")]
    ; Crear strings HTML para cada sección
    (concat
      (create-html-header token_lst)
      (create-html-meta meta_lst)
      (create-html-ingredients ingredients_lst)
      (create-html-instructions instructions_lst)
      '("</main></body></html>"))))

; --función para crear un archivo HTML dentro de la carpeta 'results' a partir de una estructura 'recipe'--
(defn create-html-file [recipe]
  (let [html-lines (html-content recipe)]
    (if (empty? html-lines)
      (println "No se pudo crear el archivo HTML: la receta está vacía.")
      (do
        (create-styles/create-styles-file) ; Asegurarse de que los estilos estén creados
        (write-html-file html-lines)))
    (.mkdirs (io/file "results"))
    (let [max-num (get-max-recipe-num "results")
          new-num (inc max-num)
          filename (str "results/recipe" new-num ".html")
          html-with-endl (add-endl html-lines)]
      (spit filename html-with-endl)
    )))

;!TODO: modificar para que genere el html con base en la nueva estructura recipe