(def entrada "ArchivoRacket.txt")
(def contenido (slurp entrada))

(declare extraer-seccion!)

(def palabra-especial ["while" "for" "if" "elseif" "else" "function"])

; Función auxiliar para recorrer la lista plana
(defn recorrer-aux [lst acc]
  (cond
    (empty? lst) ; Caso base: la lista está vacía, se devuelve el acumulador
     (reverse acc)
    (not (some #{(first lst)} palabra-especial)) ; No es una palabra especial
     (let [elemento (first lst)]
     (cond
      (clojure.string/starts-with? elemento "//") ; Comentario de línea
       (recorrer-aux (rest lst) (cons (concat (first acc) (list elemento)) (rest acc)))
        
      (and (clojure.string/starts-with? elemento "/*") (clojure.string/ends-with? elemento "*/")) ; Comentario de bloque en un solo elemento
        (recorrer-aux (rest lst) (concat (cons (first acc) (list (list elemento))) (rest acc)))
        
      :else ; Caso recursivo: se continúa recorriendo la lista
        (let [palabra (first lst) result (extraer-seccion! palabra (rest lst) ";") ]
          (let [seccion (first result) lst (first (rest result))]
          (recorrer-aux lst (cons (cons palabra seccion) acc))))))
    
  :else ; Palabra especial
    (let [palabra (first lst) result (extraer-seccion! palabra (rest lst) "}") ]
      (let [seccion (first result) lst (first (rest result))] ;(print (format " %s" (first lst)))
      (recorrer-aux lst (cons (cons palabra seccion) acc))))))

(defn extraer-seccion! [elementoIni lst elementoFin]
  ; Función auxiliar para recorrer la sección y extraer el bloque correspondiente
  (defn loops [lst2 acc conteo]
    (cond
      (empty? lst2) (throw (RuntimeException. "Se esperaba encontrar un '}' pero no se encontró."))
      (= (first lst2) elementoFin)[(reverse (conj acc elementoFin)) (rest lst2)] ; Se encontró el final del bloque, se devuelve el acumulador y la lista actualizada
      :else ; Se encontró un elemento normal, se agrega al acumulador
        (loops (rest lst2) (cons (first lst2) acc) (+ conteo 1))))
  
  (let [[new-seccion new-list-es] (loops lst [] 1)]; Se llama a la función auxiliar para extraer la sección y se guarda el resultado en una nueva lista y la sección
    (let [lst new-list-es] ; Se asigna la nueva lista a lst para actualizar la lista en la función recorrer-aux
    [new-seccion lst]))) ; Se devuelven la sección y la lista modificada

(defn recorrer-lista [lista]
  (recorrer-aux lista [])) ; Se llama a la función auxiliar para recorrer la lista plana and obtener la lista anidada resultante.

(def string "\".*?\"")
(def comentario-bloque "/\\*[^*]*\\*+([^/*][^*]*\\*+)*/")
(def comentario-linea "\\/\\/[^#\n]*")
(def condicional "\\bif\\b")
(def condicional-elseif "\\belseif\\b")
(def condicional-else "\\belse\\b")
(def ciclo-for "for")
(def ciclo-while "while")
(def funcion "function")
(def comma "\\,")
(def llamada-funcion "call")
(def operador-condicional "(?:<=|>=|!=|==|<|>)")
(def operador-logico "\\b(?:[A|a]nd|[O|o]r|[N|n]ot)\\b")
(def tipo-variable "\\b(?:[I|i]nt|[F|f]loat|[S|s]tring)\\b")
(def identificador "\\b[a-zA-Z]\\w*\\b")
(def asignacion "=")
(def parentesis-apertura "\\(")
(def parentesis-cierre "\\)")
(def llave-apertura  "\\{")
(def llave-cierre  "\\}")
(def real "[+-]?\\s*\\d+\\s*\\.\\s*\\d+\\s*[eE]\\s*[+-]?\\s*\\d+")
(def flotante-negativo "\\-\\s*\\d+\\s*\\.\\s*\\d+")
(def flotante "[+]?\\s*\\d+\\s*\\.\\s*\\d+\\s*")
(def entero "(?:[-])?\\d+")
(def suma "\\s+\\+\\s+")
(def resta "\\s+\\-\\s+")
(def multiplicacion "\\*")
(def division "\\/")
(def potencia "\\^")
(def semicolon ";")

(def regex
   (str
    string "|"
    comentario-bloque "|"
    comentario-linea "|"
    condicional "|"
    condicional-elseif "|"
    condicional-else "|"
    ciclo-for "|"
    ciclo-while "|"
    funcion "|"
    comma "|"
    llamada-funcion "|"
    operador-logico "|"
    operador-condicional "|"
    tipo-variable "|"
    identificador "|"
    asignacion "|"
    parentesis-apertura "|"
    parentesis-cierre "|"
    llave-apertura "|"
    llave-cierre "|"
    real "|"
    flotante-negativo "|"
    flotante "|"
    entero "|"
    suma "|"
    resta "|"
    multiplicacion "|"
    division "|"
    potencia "|"
    semicolon
    ))

(def pattern (re-pattern regex))
(def matches (re-seq pattern contenido))
(defn matchesV-matchesS [matches]
  (mapv (comp first) matches))
(def matches-String (matchesV-matchesS matches))
;(print matches-String)
(def listaDeLineasEnFormato (recorrer-lista matches-String))
;(print listaDeLineasEnFormato)

(defn substring-after-match [string regex]
  (let [matcher (re-matcher (re-pattern regex) string)]
    (if-let [match (re-find matcher)]
      (let [end-index (.end matcher)]
        (subs string end-index))
      "")))

(defn agregar-token [regex label resto tokens]
  (let [matcher (re-matcher (re-pattern regex) (str (first resto)))]
    (if-let [match (re-find matcher)]
      [(cons [match label] tokens)
       (if (= (first resto) "")
         (rest resto)
         (cons (substring-after-match (first resto) tipo-variable ) (rest resto)))]
      [tokens (rest resto)])))

(defn lexico [texto]
  (loop [tokens [] resto texto]
    ;(println (str resto))
    (cond
      (empty? resto) (reverse tokens)

      (re-matches (re-pattern string) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token string "string" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern comentario-bloque) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token comentario-bloque "comentario-bloque" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern comentario-linea) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token comentario-linea "comentario-linea" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern condicional) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token condicional "if" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern condicional-elseif) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token condicional-elseif "elseif" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern condicional-else) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token condicional-else "else" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern ciclo-for) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token ciclo-for "for" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern ciclo-while) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token ciclo-while "while" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern funcion) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token funcion "function" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern comma) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token comma "comma" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern llamada-funcion) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token llamada-funcion "llamada-funcion" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern operador-logico) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token operador-logico "operador-logico" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern operador-condicional) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token operador-condicional "operador-condicional" resto tokens)]
        (recur new-tokens new-resto))
      
      (re-matches (re-pattern tipo-variable) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token tipo-variable "tipo-variable" resto tokens)]
        (recur new-tokens new-resto))
      
      (re-matches (re-pattern identificador) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token identificador "variable" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern asignacion) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token asignacion "asignacion" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern parentesis-apertura) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token parentesis-apertura "parentesis-apertura" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern parentesis-cierre) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token parentesis-cierre "parentesis-cierre" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern llave-apertura) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token llave-apertura "llave-apertura" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern llave-cierre) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token llave-cierre "llave-cierre" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern real) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token real "real" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern flotante-negativo) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token flotante-negativo "flotante-negativo" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern flotante) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token flotante "flotante" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern entero) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token entero "entero" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern suma) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token suma "suma" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern resta) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token resta "resta" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern multiplicacion) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token multiplicacion "multiplicacion" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern division) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token division "division" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern potencia) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token potencia "potencia" resto tokens)]
        (recur new-tokens new-resto))

      (re-matches (re-pattern semicolon) (str (first resto)))
      (let [[new-tokens new-resto] (agregar-token semicolon "semicolon" resto tokens)]
        (recur new-tokens new-resto))
      :else (recur tokens (rest resto)))))


(def tokens (map lexico listaDeLineasEnFormato))
;(print tokens)

(def i (atom -1))
(def token (atom "valor inicial"))
(def lastToken (atom "initial"))

(defn show-error [expected token]
  (println (format  "\nERROR: esperaba %s, recibió %s" expected token)))

(declare es-expresion es-factor es-sentencia es-bloque es-condicional es-ciclo-for es-ciclo-while 
         es-declaracion-funcion es-expresion-for es-parametros-funcion-loop)

(defn sigToken [lineaToken]
  (swap! i inc)
  (let [result (if (< @i (count lineaToken))
                     (do
                       (print (first (nth lineaToken @i)))
                       (first (rest (nth lineaToken @i))))
                     (do (print "\n") (reset! i -1) ""))]
    result))

(defn Set-SigToken [LastToken lineaToken]
  (reset! lastToken LastToken)
  (reset! token (sigToken lineaToken)))

(defn es-factor [lineaToken]
  (cond
    (= @token "parentesis-apertura")
      (do
        (Set-SigToken "expresion" lineaToken)
        (es-expresion lineaToken)))
      (cond
        (= @token "parentesis-cierre")
          (Set-SigToken "expresion" lineaToken)
        (some  #{@token} ["variable" "entero" "real" "flotante" "flotante-negativo"])
          (Set-SigToken "expresion" lineaToken)))

(defn es-terminoprime [lineaToken]
  (cond
    (= @token "multiplicacion")
      (do
       (Set-SigToken "factor" lineaToken)
       (es-factor lineaToken)
       (es-terminoprime lineaToken))
    (= @token "division")
      (do 
       (Set-SigToken "factor" lineaToken)
       (es-factor lineaToken)
       (es-terminoprime lineaToken))
    ))

(defn es-termino [lineaToken]
  (es-factor lineaToken)
  (es-terminoprime lineaToken))

(defn es-expresionprime [lineaToken]
  (cond
    (= @token "suma")
      (do
        (Set-SigToken "termino" lineaToken)
        (es-termino lineaToken)
        (es-expresionprime lineaToken))
    (= @token "resta")
      (do
        (Set-SigToken "termino" lineaToken)
        (es-termino lineaToken)
        (es-expresionprime lineaToken))))

(defn es-expresion [lineaToken]
  (es-termino lineaToken)
  (es-expresionprime lineaToken))

(defn es-sentencia [lineaToken]
  ;(println (format "\nToken inicial: %s" @token))
  (cond
    (= @token "") ()
    (= @token "tipo-variable")
      ( do
       (Set-SigToken "variable" lineaToken)
       (if (= @token "variable")
           (Set-SigToken "asignacion" lineaToken)
           (show-error @lastToken @token))
        (if (= @token "asignacion")
           (Set-SigToken "expresion" lineaToken)
           (show-error @lastToken @token))
        (es-expresion lineaToken)
        (if (= @token "semicolon")
         (do
           (Set-SigToken "comentario-linea" lineaToken)
           (cond
              (= @token "comentario-linea")
              (do
                (Set-SigToken "sentencia" lineaToken)
                (es-sentencia lineaToken))
              (and (not (= @token "")) (not (= @token "llave-cierre")))
              (es-sentencia lineaToken)))
          (show-error @lastToken @token))
        )
    (= @token "variable")
      (do
        (Set-SigToken "asignacion" lineaToken)
        (if (= @token "asignacion")
          (Set-SigToken "expresion" lineaToken)
          (show-error @lastToken @token))
        (es-expresion lineaToken)
        (if (= @token "semicolon")
         (do
           (Set-SigToken "comentario-linea" lineaToken)
           (cond
              (= @token "comentario-linea")
              (do
                (Set-SigToken "sentencia" lineaToken)
                (es-sentencia lineaToken))
              (and (not (= @token "")) (not (= @token "llave-cierre")))
              (es-sentencia lineaToken)))
         (show-error @lastToken @token))
        )
    (= @token "comentario-linea")
      (do
       (Set-SigToken "sentencia" lineaToken)
       (es-sentencia lineaToken))
    (= @token "comentario-bloque")
      (do
        (Set-SigToken "sentencia" lineaToken)
        (es-sentencia lineaToken))
    (= @token "for")
      (do
        (reset! lastToken "for")
        (es-ciclo-for lineaToken)
        (es-sentencia lineaToken))
    (= @token "while")
      (do
        (reset! lastToken "while")
        (es-ciclo-while lineaToken)
        (es-sentencia lineaToken))
    (= @token "if")
      (do
        (reset! lastToken "if")
        (es-condicional lineaToken)
        (es-sentencia lineaToken))
    (= @token "function")
      (do
        (reset! lastToken "function")
        (es-declaracion-funcion lineaToken)
        (es-sentencia lineaToken))
    (= @token "llamada-funcion")
      (do
        (reset! lastToken "call")
        (es-sentencia lineaToken))
    :else
     (println "Dato inicial no es parte del lenguaje")))

(defn es-bloque-while [lineaToken]
  (cond
    (and (= @token "llave-cierre")
         (not (some #{@token} ["tipo-variable" "variable" "comentario-linea"
                               "comentario-bloque" "for" "while" "if"
                               "function" "llamada-funcion"])))
    (when (seq @token)
      (reset! token (sigToken lineaToken)))
    :else
    (do
      (es-sentencia lineaToken)
      (es-bloque-while lineaToken))))

(defn es-bloque [lineaToken]
  (cond
    (= @token "llave-apertura")
    (do
      (reset! token (sigToken lineaToken))
      (es-bloque-while lineaToken))
    :else
    (show-error "llave-apertura" @token)))

(defn es-condicion [lineaToken]
  (when (some #{@token} ["variable" "entero" "flotante" "flotante-negativo" "real"])
    (reset! token (sigToken lineaToken)))
   (when (= @token "operador-condicional")
    (reset! token (sigToken lineaToken)))
  (when (some #{@token} ["variable" "entero" "flotante" "flotante-negativo" "real"])
    (reset! token (sigToken lineaToken)))
  (when (= @token "operador-logico")
    (reset! token (sigToken lineaToken))
    (es-condicion lineaToken))
  )

(defn es-condicional-else [lineaToken]
  (when (= @token "else")
    (reset! token (sigToken lineaToken))
    (es-bloque lineaToken)
    ))

(defn es-condicional-elseif [lineaToken]
  (when (= @token "elseif")
    (reset! token (sigToken lineaToken)))
  (when (= @token "parentesis-apertura")
    (reset! token (sigToken lineaToken))
    (es-condicion lineaToken))
  (when (= @token "parentesis-cierre")
    (reset! token (sigToken lineaToken))
    (es-bloque lineaToken)))

(defn es-condicional [lineaToken]
  (when (= @token "if")
    (Set-SigToken "parentesis-apertura" lineaToken))
  (when (= @token "parentesis-apertura")
      (Set-SigToken "condicion" lineaToken)
      (es-condicion lineaToken))
  (when (= @token "parentesis-cierre")
      (Set-SigToken "bloque" lineaToken)
      (es-bloque lineaToken)
      (loop []
        (cond
          (= @token "elseif")
            (do
              (es-condicional-elseif lineaToken)
              (recur))
         (= @token "else")
          (es-condicional-else lineaToken)))))

(defn es-ciclo-for [lineaToken]
  (cond
    (= @token "for")
    (do
     (Set-SigToken "parentesis-apertura" lineaToken)
     (if (= @token "parentesis-apertura")
         (Set-SigToken "tipo-variable" lineaToken)
         (show-error @lastToken @token))
     (if (= @token "tipo-variable")
         (Set-SigToken "variable" lineaToken)
         (show-error @lastToken @token))
     (if (= @token "variable")
         (do
           (Set-SigToken "semicolon" lineaToken)
           (when (= @token "asignacion")(reset! token (sigToken lineaToken)))
           (when (some #{@token} ["variable""entero""flotante""real""flotante-negativo"])
             (reset! token (sigToken lineaToken)))
           )
         (show-error lastToken token))
     (if (= @token "semicolon")
         (Set-SigToken "condicion" lineaToken)
         (show-error @lastToken @token))
     (es-condicion lineaToken)
     (if (= @token "semicolon")
         (Set-SigToken "expresion-for" lineaToken)
         (show-error @lastToken @token))
     (es-expresion-for lineaToken)
     (if (= @token "parentesis-cierre")
         (Set-SigToken "bloque" lineaToken)
         (show-error @lastToken @token))
     (es-bloque lineaToken)
      )
    :else
     (show-error @lastToken @token)))

(defn es-expresion-for [lineaToken]

  (when (= @token "variable")
    (Set-SigToken "asignacion" lineaToken))
  (when (= @token "asignacion")
    (Set-SigToken "variable" lineaToken))
  (when (= @token "variable")
    (Set-SigToken "suma" lineaToken))
  (when (= @token "suma")
    (Set-SigToken "suma" lineaToken))
  (when (= @token "suma")
    (Set-SigToken "resta" lineaToken))
  (when (= @token "resta")
    (Set-SigToken "resta" lineaToken))
  (when (= @token "resta")
    (Set-SigToken "factor" lineaToken))
  (when (some #{@token} ["entero" "variable" "flotante" "flotante-negativo" "real"])
    (Set-SigToken "semicolon" lineaToken))
  (when (= @token "semicolon")
    (Set-SigToken "parentesis-cierre" lineaToken))
  )

(defn es-parametros-funcion [lineaToken]
  (cond
    (= @token "tipo-variable")
    (do
     (Set-SigToken "variable" lineaToken)
     (if (= @token "variable")
         (Set-SigToken "parentesis-cierre" lineaToken)
         (show-error @lastToken @token))
     (es-parametros-funcion-loop lineaToken))
    :else
     (show-error @lastToken @token)))

(defn es-parametros-funcion-loop [lineaToken]
  (cond
    (= @token "comma")
     (do 
      (Set-SigToken "tipo-variable" lineaToken)
      (if (= @token "tipo-variable")
         (Set-SigToken "variable" lineaToken)
         (show-error @lastToken @token))
      (if (= @token "variable")
         (Set-SigToken "paramentros-loop" lineaToken)
         (show-error @lastToken @token))
      (es-parametros-funcion-loop lineaToken))))

(defn es-ciclo-while [lineaToken]
  (cond
    (= @token "while")
    (do
     (Set-SigToken "parentesis-apertura" lineaToken)
     (if (= @token "parentesis-apertura")
         (Set-SigToken "condicion" lineaToken)
         (show-error @lastToken @token))
     (es-condicion lineaToken)
     (if (= @token "parentesis-cierre")
         (Set-SigToken "bloque" lineaToken)
         (show-error @lastToken @token))
     (es-bloque lineaToken))
    :else
     (show-error @lastToken @token)))


(defn es-declaracion-funcion [lineaToken]
  (cond
   (= @token "function")
    (do
     (Set-SigToken "variable" lineaToken)
     (if (= @token "variable")
         (Set-SigToken "parentesis-apertura" lineaToken)
         (show-error @lastToken @token))
     (if (= @token "parentesis-apertura")
         (Set-SigToken "parametros" lineaToken)
         (show-error @lastToken @token))
     (es-parametros-funcion lineaToken)
     (if (= @token "parentesis-cierre")
         (Set-SigToken "bloque" lineaToken)
         (show-error @lastToken @token))
     (es-bloque lineaToken))
    :else
     (show-error @lastToken @token)))

(defn check-token []

  (dorun (map (fn [x]
    (do
        (reset! token (sigToken x))
        (es-sentencia x)))
  tokens))
  
  (if (= @token "")
        (println "\nOKS")
      (do
        (show-error @lastToken @token)
        (println "\nNOPE")
        )))


(check-token)