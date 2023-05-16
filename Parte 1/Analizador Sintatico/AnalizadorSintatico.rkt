#lang racket




(define tokens '(("Int" "tipo_Variable") ("b" "variable") ("=" "asignacion") ("7" "entero") ("Float" "tipo_Variable") ("a" "variable") ("=" "asignacion") ("32.4" "flotante") ("*" "multiplicacion") ("(" "parentesis_apertura") ("-8.6" "resta") ("-" "resta") ("b" "variable") (")" "parentesis_cierre") ("/" "division") ("6.1E-8" "real") (";" "semicolon") ))

(define (recorre_tokens tokens)
  (cond ((null? tokens) '())
        ((null? (cdr tokens)) (display (cdar tokens))(newline))
        (else (recorre_tokens (cdr tokens))
              (display (cdar tokens)) ; Mostramos el elemento actual
              (newline)))) ; Agregamos una nueva línea para separar los elementos

(recorre_tokens tokens)