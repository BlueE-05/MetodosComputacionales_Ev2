#lang racket

(provide tokenize lexico)

(define match-pos regexp-match-positions) ; acortar el nombre de la función regexp-match-positions

; --definición de léxico--
(define lexico
  (list
   (list 'rg_comment          #rx"^//[ ]*.*")
   (list 'rg_char             #rx"^\"(.)\"")
   (list 'rg_state            #rx"^q[0-9]+")
   (list 'rg_DFAname          #rx"DFA|dfa")
   (list 'rg_PDAname          #rx"PDA|pda")
   (list 'rg_alphabet         #rx"^alphabet")
   (list 'rg_statesDef        #rx"^states")
   (list 'rg_accept           #rx"^accept")
   (list 'rg_transitions      #rx"^transitions")
   (list 'rg_int              #rx"^[0-9]+")
   (list 'rg_identifier       #rx"^[a-zA-Z_][a-zA-Z0-9_]*")
   (list 'rg_automatonStart   #rx"^\\{")
   (list 'rg_automatonEnd     #rx"^\\}")
   (list 'rg_colon            #rx"^::")
   (list 'rg_parentesisStart  #rx"^\\(")
   (list 'rg_parentesisEnd    #rx"^\\)")
   (list 'rg_comma            #rx"^,")
   (list 'rg_listStart        #rx"^\\[")
   (list 'rg_listEnd          #rx"^\\]")
   (list 'rg_action           #rx"^->")
  )
)

; --funciones para tokenizar--
; Función recursiva que busca la primera coincidencia de un patrón en una cadena
(define (match-first str)
  (define (aux lst)
    (cond
      [(null? lst) #f]
      [else
       (define name (symbol->string (first (car lst)))) ; convertir a string aquí
       (define rgx  (second (car lst)))
       (define m (regexp-match rgx str))
       (if m
           (list name (first m)) ; ahora name es string
           (aux (cdr lst)))]))
  (aux lexico))

; Función recursiva que tokeniza una cadena
(define (tokenize str)
  (cond
    [(string=? (string-trim str) "") '()]
    [else
     (define s (string-trim str))
     (define m (match-first s))
     (if m
         (let* ([tok (second m)]
                [rest (substring s (string-length tok))])
           (cons m (tokenize rest)))
         (let ([error-char (substring s 0 1)])
           (cons (list "rg_error" error-char)
                 (tokenize (substring s 1)))))]))