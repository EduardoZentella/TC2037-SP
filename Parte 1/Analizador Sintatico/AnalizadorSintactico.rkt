#lang racket
#|(define tokens
    '(("Int" "tipo_Variable") ("a" "variable")
    ("=" "asignacion") ("7" "entero") (";" "semicolon") ("// Comentario" "comentario_linea")
    ))|#
    
#|(define tokens
    '(("a" "variable")
    ("=" "asignacion") ("7" "entero") (";" "semicolon") ("// Comentario" "comentario_linea")
    ))|#

#|(define tokens 
  '(("if" "if") ("(" "parentesis_apertura") ("i" "variable") 
    ("<" "operador_cond") ("a" "variable") ("AND" "operador_log")
    ("b" "variable") (">" "operador_cond") ("c" "variable")(")" "parentesis_cierre")
    ("{" "broche_apertura") ("Int" "tipo_Variable") ("a" "variable") 
    ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre") ("elseif" "elseif") ("(" "parentesis_apertura")
    ("i" "variable") ("<" "operador_cond") ("a" "variable") (")" "parentesis_cierre")
    ("{" "broche_apertura") ("Int" "tipo_Variable") ("a" "variable")
    ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre") ("else" "else") ("{" "broche_apertura")
    ("Int" "tipo_Variable") ("a" "variable") ("=" "asignacion")
    ("7" "entero") (";" "semicolon") ("}" "broche_cierre")))|#

#|(define tokens 
  '(("function" "function") ("variable" "variable") ("(" "parentesis_apertura") 
    ("Int" "tipo_Variable") ("a" "variable") ("," "comma") 
    ("Float" "tipo_Variable") ("b" "variable") 
    (")" "parentesis_cierre") ("{" "broche_apertura") ("Int" "tipo_Variable") 
    ("a" "variable") ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre")
    ))|#
    
#|(define tokens 
  '(("for" "for") ("(" "parentesis_apertura")("Int" "tipo_variable") ("a" "variable") 
    (";" "semicolon") ("a" "variable") ("<" "operador_cond") 
    ("10" "entero") (";" "semicolon") ("a++" "variable")
    (")" "parentesis_cierre") ("{" "broche_apertura") ("Int" "tipo_Variable") 
    ("a" "variable") ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre")
    ))|#
    
#|(define tokens 
  '(("while" "while") ("(" "parentesis_apertura") ("a" "variable") ("<" "operador_cond") 
    ("10" "entero") (")" "parentesis_cierre") ("{" "broche_apertura") ("Int" "tipo_Variable") 
    ("a" "variable") ("=" "asignacion") ("7" "entero") (";" "semicolon")
    ("}" "broche_cierre")
    ))|#
    
#|(define tokens 
  '(("// Esto es un comentario" "comentario_linea") 
    ))|#

(define tokens 
  '(("Int" "tipo-variable") ("a" "variable") ("=" "asignacion") ("3"  "entero") ("+" "suma") ("2" "entero") (";"  "semicolon") ("// Comentario largo y opcional"
  "comentario-linea") ("Int" "tipo-variable") ("b" "variable") ("=" "asignacion") ("33" "entero") (";" "semicolon") ("//Comentario91292394"
  "comentario-linea") ("/*



Comentario



*/" "comentario-bloque") ("// Prueba "
 "comentario-linea") ("a" "variable") ("=" "asignacion") (
"1.23e12" "real") ("+" "suma") ("-23.3" "flotante-negativo") ("/" "plas") ("32.2" "flotante") (";" "semicolon")))
    
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
                       (cadr (list-ref tokens i)))
                     '(""))))
    (set! token result)
    result))

(define (es-condicional-else)
    (when (equal? token "else")
      (set! token (sigToken))
      (es-bloque)))


(define (es-condicional-elseif)
  (when (equal? token "elseif")
    (set! token (sigToken)))
  (when (equal? token "parentesis-apertura")
    (set! token (sigToken))
    (es-condicion))
  (when (equal? token "parentesis-cierre")
    (set! token (sigToken))
    (es-bloque)))


(define (es-condicional)
  (when (eq? token "if")
    (set! token (sigToken))
    (when (eq? token "parentesis-apertura")
      (set! token (sigToken))
      (es-condicion)
      (when (eq? token "parentesis-cierre")
        (set! token (car (sigToken)))
        (es-bloque)
        (let loop ()
          (when (eq? token "elseif")
            (es-condicional-elseif)
            (loop))
          (when (eq? token "else")
            (es-condicional-else)))))))

(define (es-condicion)
  (when (member token '("variable" "entero" "flotante" "flotante-negativo" "real"))
    (set! token (sigToken)))
  (when (eq? token "operador-condicional")
    (set! token (sigToken)))
  (when (member token '("variable" "entero" "flotante" "flotante-negativo" "real"))
    (set! token (sigToken)))
  (when (eq? token "operador-logico")
    (set! token (sigToken))
    (es-condicion)))



(define (es-ciclo-for)
  (cond
    [(equal? token "for")
     (set! token (sigToken))
     (if (equal? token "parentesis-apertura")
         (set! token (sigToken))
         (show-error "parentesis-apertura" token))
     (if (equal? token "tipo_variable")
         (set! token (sigToken))
         (show-error "tipo-variable" token))
     (if (equal? token "variable")
         (set! token (sigToken))
         (show-error "variable" token))
     (if (equal? token "semicolon")
         (set! token (sigToken))
         (show-error "semicolon" token))
     (es-condicion)
     (if (equal? token "semicolon")
         (set! token (sigToken))
         (show-error "semicolon" token))
     (es-expresion)
     (if (equal? token "parentesis-cierre")
         (set! token (sigToken))
         (show-error "parentesis-cierre" token))
     (es-bloque)]
    [else
     (show-error "for" token)]))


(define (es-parametros-funcion)
  (cond
    [(equal? token "tipo-variable")
     (set! token (sigToken))
     (if (equal? token "variable")
         (set! token (sigToken))
         (show-error "variable" token))
     (es-parametros-funcion-loop)]
    [else
     (show-error "tipo-variable" token)]))

(define (es-parametros-funcion-loop)
  (cond
    [(equal? token "comma")
     (set! token (sigToken))
     (if (equal? token "tipo-variable")
         (set! token (sigToken))
         (show-error "tipo_variable" token))
     (if (equal? token "variable")
         (set! token (sigToken))
         (show-error "variable" token))
     (es-parametros-funcion-loop)]
    [else
     (void)]))

(define (es-ciclo-while)
  (cond
    [(equal? token "while")
     (set! token (sigToken))
     (if (equal? token "parentesis-apertura")
         (set! token (sigToken))
         (show-error "parentesis-apertura" token))
     (es-condicion)
     (if (equal? token "parentesis-cierre")
         (set! token (sigToken))
         (show-error "parentesis-cierre" token))
     (es-bloque)]
    [else
     (show-error "while" token)]))


(define (es-declaracion-funcion)
  (cond
    [(equal? token "function")
     (set! token (sigToken))
     (if (equal? token "variable")
         (set! token (sigToken))
         (show-error "variable" token))
     (if (equal? token "parentesis-apertura")
         (set! token (sigToken))
         (show-error "parentesis-apertura" token))
     (es-parametros-funcion)
     (if (equal? token "parentesis-cierre")
         (set! token (sigToken))
         (show-error "parentesis-cierre" token))
     (es-bloque)]
    [else
     (show-error "function" token)]))


(define (es-sentencia)
  ;(displayln (string-append "\nToken inicial: " token))
  (cond
    [(equal? token "tipo-variable")
     (set! last 'tipo_Variable)
     (set! token (sigToken))
     (set! last 'variable)
     (if (equal? token "variable")
         (set! token (sigToken))
         (show-error last token))
     (set! last 'asignacion)
     (if (equal? token "asignacion")
         (set! token (sigToken))
         (show-error last token))
     (set! last 'expresion)
     (es-expresion)
     (set! last 'semicolon)
     (if (equal? token "semicolon")
         (begin
           (set! token (sigToken))
           (set! last 'Comentario_linea)
           (cond [(equal? token "comentario-linea")
                (set! token (sigToken))
                (es-sentencia)]))
         (show-error last token))]
    [(equal? token "variable")
     (set! last 'variable)
     (set! token (sigToken))
     (set! last 'asignacion)
     (if (equal? token "asignacion")
         (set! token (sigToken))
         (show-error last token))
     (set! last 'expresion)
     (es-expresion)
     (set! last 'semicolon)
     (if (equal? token "semicolon")
         (begin
           (set! token (sigToken))
           (set! last 'Comentario_linea)
           (cond [(equal? token "comentario-linea")
                (set! token (sigToken))
                (es-sentencia)]))
        (show-error last token))]
    [(equal? token "comentario-linea")
     (set! last 'Comentario_linea)
     (set! token (sigToken))
     (es-sentencia)]
    [(equal? token "comentario-bloque")
     (set! last 'Comentario_bloque)
     (set! token (sigToken))
     (es-sentencia)]
    [(equal? token "for")
     (set! last 'for)
     (es-ciclo-for)
     (es-sentencia)]
    [(equal? token "while")
     (set! last 'while)
     (es-ciclo-while)
     (es-sentencia)]
    [(equal? token "if")
     (set! last 'if)
     (es-condicional)
     (es-sentencia)]
    [(equal? token "function")
     (set! last 'function)
     (es-declaracion-funcion)
     (es-sentencia)]
    [(equal? token "llamada-funcion")
     (set! last 'llamada_funcion)
     (es-sentencia)]
    [else
     (displayln "Dato inicial no es parte del lenguaje")]))


(define (es-bloque-while)
  (cond
    [(and (equal? token "broche-cierre")
         (not (member token '("tipo-variable" "variable" "comentario-linea"
                              "comentario-bloque" "for" "while" "if"
                              "function" "llamada-funcion"))))
     (when (not(eq? token ""))
        (set! token (sigToken)))]
    [else
     (es-sentencia)
     (es-bloque-while)]))

(define (es-bloque)
  (cond
    [(equal? token "broche-apertura")
     (set! token (sigToken))
     (es-bloque-while)]
    [else
     (show-error "broche-apertura" token)]))

(define (es-terminoprime)
  (cond
    [(equal? token "multiplicacion")
     (set! token (sigToken))
     (es-factor)
     (es-terminoprime)]
    [(equal? token "division")
     (set! token (sigToken))
     (es-factor)
     (es-terminoprime)]))

(define (es-termino)
  (es-factor)
  (es-terminoprime))

(define (es-expresionprime)
  (cond
    [(equal? token "suma")
     (set! token (sigToken))
     (es-termino)
     (es-expresionprime)]
    [(equal? token "resta")
     (set! token (sigToken))
     (es-termino)
     (es-expresionprime)]))

(define (es-expresion)
  (es-termino)
  (es-expresionprime))


(define (es-factor)
  (cond
    [(equal? token "parentesis-apertura")
     (set! token (sigToken))
     (es-expresion)])
      (cond
      [(equal? token "parentesis-cierre")
        (set! token (sigToken))]
      [(or (equal? token "variable")
         (equal? token "entero")
         (equal? token "real")
         (or (equal? token "flotante") (equal? token "flotante-negativo")))
       (set! token (sigToken))]))



(define (check-token)
  (set! token (sigToken))
  (es-sentencia)
  #|(if (string=? token "")
      (begin
        (displayln "\nOKS")
        (newline)
        )
      (begin
        (show-error last token)
        (displayln "\nNOPE")
        (newline))))|#)


(check-token)