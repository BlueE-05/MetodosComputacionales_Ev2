#lang racket

(provide create-automata run-automata)

(struct automata (type states accept start transitions) #:transparent)

(define automatas (make-hash))

(define (create-states n)
  (define (aux i acc)
    (if (< i 0)
        acc
        (aux (- i 1) (cons (string->symbol (format "q~a" i)) acc))))
  (aux (- n 1) '()))

(define (init-trans-table states table)
  (if (null? states)
      table
      (begin
        (hash-set! table (car states) '())
        (init-trans-table (cdr states) table))))

(define (add-transitions transitions table)
  (cond
    [(null? transitions) table]
    [else
     (match (car transitions)
       [(list desde chars hacia)
        (define (agregar-chars cs)
          (cond
            [(null? cs) '()]
            [else
             (define actuales (hash-ref table desde))
             (hash-set! table desde (cons (cons (car cs) hacia) actuales))
             (agregar-chars (cdr cs))]))
        (agregar-chars chars)] )
     (add-transitions (cdr transitions) table)]))

(define (create-automata name type state_qty accept_state transitions [alfabeto '()])
  (define states (create-states state_qty))
  (define start (car states))
  (define accept-syms (map string->symbol accept_state))
  (define transitions-syms
    (map (lambda (t)
           (match t
             [(list from chars to)
              (list (string->symbol from) chars (string->symbol to))]))
         transitions))
  (define trans-table (make-hash))
  (init-trans-table states trans-table)
  (add-transitions transitions-syms trans-table)
  (define aut (automata type states accept-syms start trans-table))
  (hash-set! automatas name aut)
  (printf "[✓] Automaton ~a created successfully.\n" name))

(define (input->string lst)
  (string-append "[" (string-join (map ~a lst) " ") "]"))

(define (run-automata name input)
  (define aut (hash-ref automatas name #f))
  (define result
    (if (not aut)
        (format "[!] Automaton ~a not found." name)
        (match aut
          [(automata type states accept start transitions)
           (define (traverse state input)
             (cond
               [(null? input)
                (if (member state accept) 'success 'failure)]
               [else
                (define options (hash-ref transitions state '()))
                (define next (assoc (car input) options))
                (if next
                    (traverse (cdr next) (cdr input))
                    'failure)]))
           (define res (traverse start input))
           (format "~a with input ~a -> ~a" name (input->string input) res)])))
  result)