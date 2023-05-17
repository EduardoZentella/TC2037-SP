"""
Este archivo obtiene los tokens del analizador lexico y  
realiza el analisis sintatico, esto comprobara que las
instrucciones sean factibles.

"""

tokens = [('Int', 'tipo_Variable'), ('b', 'variable'), ('=', 'asignacion'), ('7', 'entero'), (';','semicolon'),('Float', 'tipo_Variable'), ('a', 'variable'), ('=', 'asignacion'), (' 32.4 ', 'flotante'), ('*', 'multiplicacion'), ('(', 'parentesis_apertura'), ('-8.6', 'flotante_negativo'), ('-', 'resta'), ('b', 'variable'), (')', 'parentesis_cierre'), ('/', 'division'), (' 6.1E-8', 'real'), (';', 'semicolon'), ('d', 'variable'), ('=', 'asignacion'), ('a', 'variable'), ('^', 'potencia'), ('b', 'variable'), (';', 'semicolon'), ('// Esto es un comentario', 'comentario_linea'), ('x_33', 'variable'), ('=', 'asignacion'), ('8', 'entero'), ('z3Z', 'variable'), ('=', 'asignacion'), (';', 'semicolon'), ('// bruh', 'comentario_linea'), ('y', 'variable'), ('=', 'asignacion'), ('" un string"', 'string'), ('a235cz', 'variable'), ('=', 'asignacion'), ('a', 'variable'), ('*', 'multiplicacion'), ('c', 'variable'), ('// 32.3', 'comentario_linea'), ('/* Comentario */', 'comentario_bloque'), ('/* int = 23 s\n */', 'comentario_bloque'), ('String', 'tipo_Variable'), ('a', 'variable'), ('=', 'asignacion'), ('b', 'variable'), (';', 'semicolon'), ('-6.8', 'flotante_negativo'), ('- 6.8', 'flotante_negativo')]



# Lenguaje a utilizar
"""

<sentencia> -> <tipo_variable> <variable> <asignacion> <expresion> <Semicolon> <comentario_linea>?| <variable> <asignacion> <expresion> <Semicolon> | <Comentario_linea> | <Comentario_bloque> |<ciclo_for> | <ciclo_while> | <condicional> | <declaracion_funcion> | <llamada_funcion> 

<declaracion_funcion> -> "function" <variable> <parentesis_apertura> <parametros_funcion> <parentesis_cierre> <bloque>

<parametros_funcion> -> <tipo_variable> <variable> ("," <tipo_variable> <variable>)* | epsilon

<ciclo_for> -> "for" <parentesis_apertura> <variable> <Semicolon> <condicion> <Semicolon> <expresion> <parentesis_cierre> <bloque>

<ciclo_while> -> "while" <parentesis_apertura> <condicion> <parentesis_cierre> <bloque>

<condicion> -> <variable> <Operador_Condicional> <variable> (<Operador_Logico> <condicion>)*

<condicional> -> "if" <parentesis_apertura> <condicion> <parentesis_cierre> <bloque> <condicional_elseif>* <condicional_else>?

<condicional_elseif> -> "elseif" <parentesis_apertura> <expresion> <parentesis_cierre> <bloque>

<condicional_else> -> "else" <bloque>

<bloque> -> "{" <sentencia>* "}"

<expresion> -> <termino> <expresion'>

<expresion'> -> <suma> <termino> <expresion'> | <resta> <termino> <expresion'> | epsilon

<termino> -> <factor> <termino'>

<termino'> -> <multiplicacion> <factor> <termino'> | <division> <factor> <termino'> | epsilon

<factor> -> <parentesis_apertura> <expresion> <parentesis_cierre> | <variable> | <entero> | <real> | <flotante>

<flotante> -> "flotante" | "flotante_negativo"

<variable> -> "variable"

<asignacion> -> "asignacion"

<entero> -> "entero"

<real> -> "real"

<tipo_variable> -> "tipo_Variable"

<Semicolon> -> "semicolon"

<Comentario_linea> -> "comentario_linea"

<Comentario_bloque> -> "comentario_bloque"

<Operador_Condicional> -> "operador_cond"

<Operador_Logico> -> "operador_log"

<parentesis_apertura> -> "parentesis_apertura"

<parentesis_cierre> -> "parentesis_cierre"

"""

def sigToken():
    global i
    global tokens
    i = i + 1
    if i < len(tokens):
        print(tokens[i][0])
        return tokens[i][1]
    else:
        return ""

def showError(expected, token):
    print("ERROR: esperaba ", expected, ", recibio ", token)

def es_sentencia():
    global token
    global last
    print("Token inicial: " + token)
    if token == "tipo_Variable":
        last = "tipo_Variable"
        token = sigToken()
        last = "variable"
        if token == "variable":
            token = sigToken()
        else:
            showError(last, token)
        last = "asignacion"
        if token == "asignacion":
            token = sigToken()
        else:
            showError(last, token)
        last = "expresion"
        es_expresion()
        last = "Semicolon"
        if token == "semicolon":
            token = sigToken()
            last = "Comentario_linea"
            if token == "Comentario_linea":
                token = sigToken()
        else:
            showError(last, token)
    elif token == "variable":
        last = "variable"
        token = sigToken()
        last = "asignacion"
        if token == "asignacion":
            token = sigToken()
        else:
            showError(last, token)
        last = "expresion"
        es_expresion()
        last = "semicolon"
        if token == "semicolon":
            token = sigToken()
        else:
            showError(last, token)
    elif token == "Comentario_linea":
        last = "Comentario_linea"
        token = sigToken()
    elif token == "Comentario_bloque":
        last = "Comentario_bloque"
        token = sigToken()
    elif token == "for":
        last = "for"
        es_ciclo_for()
    elif token == "while":
        last = "while"
        es_ciclo_while()
    elif token == "if":
        last = "if"
        es_condicional()
    elif token == "function":
        last = "function"
        es_declaracion_funcion()
    elif token == "llamada_funcion":
        last = "llamada_funcion"
    else:
        print("Dato inicial no es parte del lenguaje")

def es_declaracion_funcion():
    global token
    if token == "function":
        token = sigToken()
        if token == "variable":
            token = sigToken()
        if token == "parentesis_apertura":
            token = sigToken()
            es_parametros_funcion()
            if token == "parentesis_cierre":
                token = sigToken()
                es_bloque()

def es_parametros_funcion():
    global token
    if token == "tipo_Variable":
        token = sigToken()
        if token == "variable":
            token = sigToken()
        while token == "comma":
            token = sigToken()
            if token == "tipo_Variable":
                token = sigToken()
            if token == "variable":
                token = sigToken()

def es_ciclo_for():
    global token
    if token == "for":
        token = sigToken()
    if token == "parentesis_apertura":
        token = sigToken()
    if token == "tipo_variable":
        token = sigToken()
    if token == "variable":
        token = sigToken()
    if token == "semicolon":
        token = sigToken()
        es_condicion()
    if token == "semicolon":
        token = sigToken()
        es_expresion()
    if token == "parentesis_cierre":
        token = sigToken() 
        es_bloque()

def es_ciclo_while():
    global token
    if token == "while":
        token = sigToken()
        if token == "parentesis_apertura":
            token = sigToken()
            es_condicion()
            if token == "parentesis_cierre":
                token = sigToken()
                es_bloque()

def es_condicion():
    global token
    print("Condicion inicial: " + token)
    if token == "variable":
        token = sigToken()
    if token == "operador_cond":
        token = sigToken()
    if token == "variable":
        token = sigToken()
    while token == "operador_log":
        token = sigToken()
        es_condicion()

def es_condicional():
    global token
    if token == "if":
        token = sigToken()
        if token == "parentesis_apertura":
            token = sigToken()
            es_condicion()
            if token == "parentesis_cierre":
                token = sigToken()
                es_bloque()
                while token == "elseif":
                    es_condicional_elseif()
                if token == "else":
                    es_condicional_else()

def es_condicional_elseif():
    global token
    if token == "elseif":
        token = sigToken()
        if token == "parentesis_apertura":
            token = sigToken()
            es_condicion()
            if token == "parentesis_cierre":
                token = sigToken()
                es_bloque()

def es_condicional_else():
    global token
    if token == "else":
        token = sigToken()
        es_bloque()

def es_bloque():
    global token
    if token == "broche_apertura":
        token = sigToken()
        while (token != "broche_cierre" and 
               token in ["tipo_Variable", "variable", "comentario_linea", 
                         "comentario_bloque", "for", "while", "if", 
                         "function", "llamada_funcion"]):
            es_sentencia()
        if token == "broche_cierre":
            token = sigToken()

def es_expresion():
    global token
    es_termino()
    es_expresionprime()

def es_expresionprime():
    global token
    if token == "suma":
        token = sigToken()
        es_termino()
        es_expresionprime()
    elif token == "resta":
        token = sigToken()
        es_termino()
        es_expresionprime()

def es_termino():
    global token
    es_factor()
    es_terminoprime()

def es_terminoprime():
    global token
    if token == "multiplicacion":
        token = sigToken()
        es_factor()
        es_terminoprime()
    elif token == "division":
        token = sigToken()
        es_factor()
        es_terminoprime()

def es_factor():
    global token
    if token == "parentesis_apertura":
        token = sigToken()
        es_expresion()
        if token == "parentesis_cierre":
            token = sigToken()
    elif token == "variable":
        token = sigToken()
    elif token == "entero":
        token = sigToken()
    elif token == "real":
        token = sigToken()
    elif token == "flotante" or token == "flotante_negativo":
        token = sigToken()

i = -1
token = sigToken()
es_sentencia()
if token == "":
    print("OKS\n")
else:
    showError(last, token)
    print("NOPE\n")
