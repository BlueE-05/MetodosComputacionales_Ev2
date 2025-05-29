#lang racket

(provide create-css create-html)

(define css-output
  (string-append
   "text { font-family: \\\"Courier New\\\", monospace; font-size: 18px; }\n\n"
   "body { background-color: #1E1E2E; }\n\n"
   ".indent { display: block; margin-left: 2em; }\n\n"
   ".braces,\n.colon,\n.comma { color: #A4D99C; font-weight: bold; }\n\n"
   ".parentesis { color: #F28DB2; font-weight: bold; }\n\n"
   ".chars { color: #F2DBAE; }\n\n"
   ".identifiers { color: #89B4FA; font-style: italic; }\n\n"
   ".ints { color: #CDD6F4; }\n\n"
   ".keywords { color: #C6A7F2; font-weight: bold; }\n\n"
   ".comments { color: #72788C; font-style: italic; }\n\n" 
   ".error { text-decoration: underline wavy #F5403B; color: #72788C; font-style: italic; }\n\n"".fixed-footer { position: fixed; bottom: 0; left: 0; width: 100%; height: 25vh; background-color: #121217; outline: 1px solid #CDD6F4; box-sizing: border-box; padding: 0.5em 1em; color: #CDD6F4; font-family: \"Courier New\", monospace; display: flex; flex-direction: column; }\n\n"
   ".footer-title { font-size: 0.9rem; color: #89B4FA; border-bottom: 1px solid #89B4FA; padding-bottom: 0.2em; margin-bottom: 0.5em; letter-spacing: 0.05em; }\n\n"
   ".terminal-text { color: #A4D99C; font-weight: bold; }"))

;; Función que escribe el CSS
(define (create-css)
  (call-with-output-file "style.css"
    (lambda (out)
      (display css-output out))
    #:exists 'replace))

; Función para convertir label a clase HTML
(define (token->class label) ; diccionario / funcion auxiliar
  (cond
    [(equal? label "rg_state") "identifiers"]
    [(equal? label "rg_statesDef") "keywords"]
    [(equal? label "rg_name") "keywords"]
    [(equal? label "rg_DFAname") "keywords"]
    [(equal? label "rg_PDAname") "keywords"]
    [(equal? label "rg_alphabet") "keywords"]
    [(equal? label "rg_accept") "keywords"]
    [(equal? label "rg_transitions") "keywords"]
    [(equal? label "rg_automatonStart") "braces"]
    [(equal? label "rg_automatonEnd") "braces"]
    
    [(equal? label "rg_identifier") "identifiers"]
    [(equal? label "rg_int") "ints"]
    [(equal? label "rg_char") "chars"]
    
    [(equal? label "rg_error") "error"]
    [(equal? label "rg_comment") "comments"]

    [(equal? label "rg_colon") "colon"]
    [(equal? label "rg_comma") "comma"]
    
    [(equal? label "rg_listStart") "parentesis"]
    [(equal? label "rg_listEnd") "parentesis"]
    
    [(equal? label "rg_parentesisStart") "parentesis"]
    [(equal? label "rg_parentesisEnd") "parentesis"]
    
    [(equal? label "rg_action") "braces"]
    
    [(equal? label "rg_resultados") "terminal-text"]
    [(equal? label "rg_resultsError") "error"]
    
    [else ""]))

; Función para convertir cada token a HTML
(define (token->html token)
  (define label (first token))
  (define value (second token))
  (format "\t<text class='~a'>~a</text>\n" (token->class label) value))

(define (token->html-enhanced tokens)
  (define (loop tokens acc in-indent)
    (cond
      [(null? tokens) ; fin de lista
       (if in-indent
           (reverse (cons "\t</div>\n" acc)) ; cerrar indentación si quedó abierta
           (reverse acc))]

      [else
       (define token (car tokens))
       (define label (first token))
       (define value (second token))

       (cond
         [(string=? label "\n")
          (loop (cdr tokens) (cons "<br>\n" acc) in-indent)]

         [(equal? label "rg_automatonStart")
          (define class (token->class label))
          (define html-line (format "\t<text class='~a'>~a</text>\n" class value))
          (loop (cdr tokens) (cons "<div class='indent'>\n" (cons html-line acc)) #t)]

         [(equal? label "rg_automatonEnd")
          (define class (token->class label))
          (define html-line (format "\t<text class='~a'>~a</text>\n" class value))
          (loop (cdr tokens) (cons html-line (cons "</div>\n" acc)) #f)]

         [else
          (define class (token->class label))
          (define html-line (format "\t<text class='~a'>~a</text>\n" class value))
          (loop (cdr tokens) (cons html-line acc) in-indent)])]))

  (apply string-append (loop tokens '() #f)))

(define (token->html-terminal tokens)
  (define (loop tokens acc in-indent)
    (cond
      [(null? tokens) ; fin de lista
       (if in-indent
           (reverse (cons "\t</div>\n" acc)) ; cerrar indentación si quedó abierta
           (reverse acc))]

      [else
       (define token (car tokens))
       (define label (first token))
       (define value (second token))

       (cond
         [(string=? label "\n")
          (loop (cdr tokens) (cons "<br>\n" acc) in-indent)]

         [else
          (define class (token->class label))
          (define html-line (format "\t<text class='~a'>~a</text>\n" class value))
          (loop (cdr tokens) (cons html-line acc) in-indent)])]))

  (apply string-append (loop tokens '() #f)))

; Generar HTML de todos los tokens
(define (html-output path tokens resultTokens)
  (string-append
    "<!DOCTYPE html>\n"
    "<html>\n"
    "<head>\n"
    "\t<meta charset=\"UTF-8\">\n"
    (format "\t<title>~a</title>\n" path)
    "\t<link rel=\"stylesheet\" href=\"style.css\">\n"
    "</head>\n"
    "<body>\n"
    ;(apply string-append (map token->html tokens))
    (token->html-enhanced tokens)
    "<div class=\"fixed-footer\">\n"
    "\t<div class=\"footer-title\">Terminal</div>"
    (token->html-terminal resultTokens)
    "</div>\n"
    "</body>\n"
    "</html>\n"))

; Función principal para crear el archivo HTML
(define (create-html path tokens resultTokens)
  (call-with-output-file path
    (lambda (out)
      (display (html-output path tokens resultTokens) out))
    #:exists 'replace))
