#lang racket
(define tokens 
  '(("if" "if") ("(" "parentesis_apertura") ("i" "variable") 
    ("<" "operador_cond") ("a" "variable") (")" "parentesis_cierre")
    ("{" "broche_apertura") ("Int" "tipo_Variable") ("a" "variable") 
    ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre") ("elseif" "elseif") ("(" "parentesis_apertura")
    ("i" "variable") ("<" "operador_cond") ("a" "variable") (")" "parentesis_cierre")
    ("{" "broche_apertura") ("Int" "tipo_Variable") ("a" "variable")
    ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre") ("else" "else") ("{" "broche_apertura")
    ("Int" "tipo_Variable") ("a" "variable") ("=" "asignacion")
    ("7" "entero") (";" "semicolon") ("}" "broche_cierre")))

(define i -1)
(define token "valor inicial")
(define last "initial")

(define (show-error expected token)
  (display "\nERROR: esperaba ")
  (display expected)
  (display ", recibió ")
  (display token))


(define (sigToken)
  (set! i (+ i 1))
  (let ((result (if (< i (length tokens))
                     (begin
                       (display (car (list-ref tokens i)))
                       (cdr (list-ref tokens i)))
                     "")))
    (set! token result)
    result))

(define (es-condicional-else)
    (when (equal? token "else")
      (set! token (car(sigToken)))
      (displayln (string-append "\nLoop else " token))
      (es-bloque)))


(define (es-condicional-elseif)
  (when (equal? token "elseif")
    (set! token (car (sigToken))))
  (when (equal? token "parentesis_apertura")
    (set! token (car (sigToken)))
    (es-condicion))
  (when (equal? token "parentesis_cierre")
    (set! token (car (sigToken)))
    (es-bloque)))


(define (es-condicional)
  (when (eq? token "if")
    (set! token (car (sigToken)))
    (when (eq? token "parentesis_apertura")
      (set! token (car (sigToken)))
      (es-condicion)
      (when (eq? token "parentesis_cierre")
        (set! token (car (sigToken)))
        (es-bloque)
        (let loop ()
          (when (eq? token "elseif")
            (es-condicional-elseif)
            (loop))
          (when (eq? token "else")
            (es-condicional-else)))))))

(define (es-condicion)
  (when (eq? token "variable")
    (set! token (car (sigToken))))
  (when (eq? token "operador_cond")
    (set! token (car (sigToken))))
  (when (eq? token "variable")
    (set! token (car (sigToken))))
  (when (eq? token "operador_log")
    (set! token (car (sigToken)))
    (es-condicion)))



(define (es-ciclo-for)
  (cond
    [(equal? token "for")
     (set! token (car(sigToken)))
     (if (equal? token "parentesis_apertura")
         (set! token (car(sigToken)))
         (show-error "parentesis_apertura" token))
     (if (equal? token "tipo_variable")
         (set! token (car(sigToken)))
         (show-error "tipo_variable" token))
     (if (equal? token "variable")
         (set! token (car(sigToken)))
         (show-error "variable" token))
     (if (equal? token "semicolon")
         (set! token (car(sigToken)))
         (show-error "semicolon" token))
     (es-condicion)
     (if (equal? token "semicolon")
         (set! token (car(sigToken)))
         (show-error "semicolon" token))
     (es-expresion)
     (if (equal? token "parentesis_cierre")
         (set! token (car(sigToken)))
         (show-error "parentesis_cierre" token))
     (es-bloque)]
    [else
     (show-error "for" token)]))


(define (es-parametros-funcion)
  (cond
    [(equal? token "tipo_Variable")
     (set! token (car(sigToken)))
     (if (equal? token "variable")
         (set! token (car(sigToken)))
         (show-error "variable" token))
     (es-parametros-funcion-loop)]
    [else
     (show-error "tipo_Variable" token)]))

(define (es-parametros-funcion-loop)
  (cond
    [(equal? token "comma")
     (set! token (car(sigToken)))
     (if (equal? token "tipo_Variable")
         (set! token (car(sigToken)))
         (show-error "tipo_Variable" token))
     (if (equal? token "variable")
         (set! token (car(sigToken)))
         (show-error "variable" token))
     (es-parametros-funcion-loop)]
    [else
     (void)]))

(define (es-ciclo-while)
  (cond
    [(equal? token "while")
     (set! token (car(sigToken)))
     (if (equal? token "parentesis_apertura")
         (set! token (car(sigToken)))
         (show-error "parentesis_apertura" token))
     (es-condicion)
     (if (equal? token "parentesis_cierre")
         (set! token (car(sigToken)))
         (show-error "parentesis_cierre" token))
     (es-bloque)]
    [else
     (show-error "while" token)]))


(define (es-declaracion-funcion)
  (cond
    [(equal? token "function")
     (set! token (car(sigToken)))
     (if (equal? token "variable")
         (set! token (car(sigToken)))
         (show-error "variable" token))
     (if (equal? token "parentesis_apertura")
         (set! token (car(sigToken)))
         (show-error "parentesis_apertura" token))
     (es-parametros-funcion)
     (if (equal? token "parentesis_cierre")
         (set! token (car(sigToken)))
         (show-error "parentesis_cierre" token))
     (es-bloque)]
    [else
     (show-error "function" token)]))


(define (es-sentencia)
  ;(displayln (string-append "\nToken inicial: " token))
  (cond
    [(equal? token "tipo_Variable")
     (set! last 'tipo_Variable)
     (set! token (car(sigToken)))
     (set! last 'variable)
     (if (equal? token "variable")
         (set! token (car(sigToken)))
         (show-error last token))
     (set! last 'asignacion)
     (if (equal? token "asignacion")
         (set! token (car(sigToken)))
         (show-error last token))
     (set! last 'expresion)
     (es-expresion)
     (set! last 'semicolon)
     (if (equal? token "semicolon")
         (begin
           (set! token (car(sigToken)))
           (set! last 'Comentario_linea)
           (if (equal? token "Comentario_linea")
               (set! token (car(sigToken)))
               (void)))
         (show-error last token))]
    [(equal? token "variable")
     (set! last 'variable)
     (set! token (car(sigToken)))
     (set! last 'asignacion)
     (if (equal? token "asignacion")
         (set! token (car(sigToken)))
         (show-error last token))
     (set! last 'expresion)
     (es-expresion)
     (set! last 'semicolon)
     (if (equal? token "semicolon")
         (set! token (car(sigToken)))
         (show-error last token))]
    [(equal? token "Comentario_linea")
     (set! last 'Comentario_linea)
     (set! token (car(sigToken)))]
    [(equal? token "Comentario_bloque")
     (set! last 'Comentario_bloque)
     (set! token (car(sigToken)))]
    [(equal? token "for")
     (set! last 'for)
     (es-ciclo-for)]
    [(equal? token "while")
     (set! last 'while)
     (es-ciclo-while)]
    [(equal? token "if")
     (set! last 'if)
     (es-condicional)]
    [(equal? token "function")
     (set! last 'function)
     (es-declaracion-funcion)]
    [(equal? token "llamada_funcion")
     (set! last 'llamada_funcion)]
    [else
     (displayln "Dato inicial no es parte del lenguaje")]))



(define (es-bloque-while)
  (cond
    [(and (not (equal? token "broche_cierre"))
          (member token '("tipo_Variable" "variable" "comentario_linea"
                           "comentario_bloque" "for" "while" "if"
                           "function" "llamada_funcion")))
     (es-sentencia)
     (es-bloque-while)]
    [(not (equal? token ""))
     (equal? token "broche_cierre")
     (displayln (string-append "\nLoop final: " token))
     (set! token (car (sigToken)))]))

(define (es-bloque)
  (cond
    [(equal? token "broche_apertura")
     (set! token (car(sigToken)))
     (es-bloque-while)]
    [else
     ; Lógica para manejar el caso en que token no es "broche_apertura"
     (show-error "broche_apertura" token)]))

(define (es-terminoprime)
  (cond
    [(equal? token "multiplicacion")
     (set! token (car(sigToken)))
     (es-factor)
     (es-terminoprime)]
    [(equal? token "division")
     (set! token (car(sigToken)))
     (es-factor)
     (es-terminoprime)]))

(define (es-termino)
  (es-factor)
  (es-terminoprime))

(define (es-expresionprime)
  (cond
    [(equal? token "suma")
     (set! token (car(sigToken)))
     (es-termino)
     (es-expresionprime)]
    [(equal? token "resta")
     (set! token (car(sigToken)))
     (es-termino)
     (es-expresionprime)]))

(define (es-expresion)
  (es-termino)
  (es-expresionprime))


(define (es-factor)
  (set! token (car(sigToken)))
  (cond
    [(equal? token "parentesis_apertura")
     (set! token (car(sigToken)))
     (es-expresion)
     (cond
       [(equal? token "parentesis_cierre")
        (set! token (car(sigToken)))])]
    [(or (equal? token "variable")
         (equal? token "entero")
         (equal? token "real")
         (or (equal? token "flotante") (equal? token "flotante_negativo")))
     (set! token (car(sigToken)))]))



(define (check-token)
  (set! token (car(sigToken)))
  (es-sentencia)
  (if (string=? token "")
      (begin
        (displayln "\nOKS")
        (newline))
      (begin
        (show-error last token)
        (displayln "\nNOPE")
        (newline))))

(check-token)