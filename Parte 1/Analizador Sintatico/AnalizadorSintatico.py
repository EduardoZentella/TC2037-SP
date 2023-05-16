"""
Este archivo obtiene los tokens del analizador lexico y  
realiza el analisis sintatico, esto comprobara que las
instrucciones sean factibles.

"""

tokens = [('Int', 'tipo_Variable'), ('b', 'variable'), ('=', 'asignacion'), (' 7', 'entero'),(';','semicolon'), ('Float', 'tipo_Variable'), ('a', 'variable'), ('=', 'asignacion'), (' 32.4 ', 'flotante'), ('*', 'multiplicacion'), ('(', 'parentesis_apertura'), ('-8.6', 'resta'), ('-', 'resta'), ('b', 'variable'), (')', 'parentesis_cierre'), ('/', 'division'), (' 6.1E-8', 'real'), (';', 'semicolon'), ('d', 'variable'), ('=', 'asignacion'), ('a', 'variable'), ('^', 'potencia'), ('b', 'variable'), (';', 'semicolon'), ('// Esto es un comentario', 'comentario_linea'), ('x_33', 'variable'), ('=', 'asignacion'), (' 8', 'entero'), ('z3Z', 'variable'), ('=', 'asignacion'), (';', 'semicolon'), ('// bruh', 'comentario_linea'), ('y', 'variable'), ('=', 'asignacion'), ('" un string"', 'string'), ('a235cz', 'variable'), ('=', 'asignacion'), ('a', 'variable'), ('*', 'multiplicacion'), ('c', 'variable'), ('// 32.3', 'comentario_linea'), ('/* Comentario */', 'comentario_bloque'), ('/* int = 23 s\n */', 'comentario_bloque'), ('String', 'tipo_Variable'), ('a', 'variable'), ('=', 'asignacion'), ('b', 'variable'), (';', 'semicolon')]


# Lenguaje a utilizar
"""

<sentencia> -> <tipo_variable> <variable> <asignacion> <expresion> <Semicolon> <comentario_linea>?| <variable> <asignacion> <expresion> <Semicolon> | <Comentario_linea> | <Comentario_bloque> |<ciclo_for> | <ciclo_while> | <condicional> | <declaracion_funcion> | <llamada_funcion> 

<declaracion_funcion> -> "function" <variable> "(" <parametros_funcion> ")" <bloque>

<parametros_funcion> -> <tipo_variable> <variable> ("," <tipo_variable> <variable>)* | epsilon

<ciclo_for> -> "for" "(" <variable> <Semicolon> <condicion> <Semicolon> <expresion> ")" <bloque>

<ciclo_while> -> "while" "(" <condicion> ")" <bloque>

<condicion> -> <variable> <Operador_Condicional> <variable> (<Operador_Logico> <condicion>)*

<condicional> -> "if" "(" <condicion> ")" <bloque> <condicional_elseif>* <condicional_else>?

<condicional_elseif> -> "elseif" "(" <expresion> ")" <bloque>

<condicional_else> -> "else" <bloque>

<bloque> -> "{" <sentencia>* "}"

<expresion> -> <termino> <expresion'>

<expresion'> -> <suma> <termino> <expresion'> | <resta> <termino> <expresion'> | epsilon

<termino> -> <factor> <termino'>

<termino'> -> <multiplicacion> <factor> <termino'> | <division> <factor> <termino'> | epsilon

<factor> -> <variable> | <entero> | <real> | <flotante>

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

"""

def sigToken():
    global i
    global tokens
    i = i + 1
    if i < len(tokens):
        print(tokens[i][1])
        return tokens[i][1]
    else:
        return ""

def showError(expected, token):
    print("ERROR: esperaba ", expected, ", recibio ", token)

def es_sentencia():
    global token
    global last
    if token == "tipo_Variable":
        last = "tipo_Variable"
        es_tipo_variable()
        last = "variable"
        es_variable()
        last = "asignacion"
        es_asignacion()
        last = "expresion"
        es_expresion()
        last = "Semicolon"
        es_Semicolon()
        if token == "Comentario_linea":
            last = "Comentario_linea"
            es_Comentario_linea()
    elif token == "variable":
        last = "variable"
        es_variable()
        last = "asignacion"
        es_asignacion()
        last = "expresion"
        es_expresion()
        last = "Semicolon"
        es_Semicolon()
    elif token == "Comentario_linea":
        last = "Comentario_linea"
        es_Comentario_linea()
    elif token == "Comentario_bloque":
        last = "Comentario_bloque"
        es_Comentario_bloque()
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

def es_declaracion_funcion():
    global token
    if token == "function":
        token = sigToken()
        es_variable()
        if token == "(":
            token = sigToken()
            es_parametros_funcion()
            if token == ")":
                token = sigToken()
                es_bloque()

def es_parametros_funcion():
    global token
    if token == "tipo_Variable":
        es_tipo_variable()
        es_variable()
        while token == ",":
            token = sigToken()
            es_tipo_variable()
            es_variable()

def es_ciclo_for():
    global token
    if token == "for":
        token = sigToken()
        if token == "(":
            token = sigToken()
            es_variable()
            if token == ";":
                token = sigToken()
                es_condicion()
                if token == ";":
                    token = sigToken()
                    es_expresion()
                    if token == ")":
                        token = sigToken() 
                        es_bloque()

def es_ciclo_while():
    global token
    if token == "while":
        token = sigToken()
        if token == "(":
            token = sigToken()
            es_condicion()
            if token == ")":
                token = sigToken()
                es_bloque()


def es_condicion():
    global token
    es_variable()
    es_Operador_Condicional()
    es_variable()
    while token == "operador_log":
        es_Operador_Logico()
        es_condicion()

def es_condicional():
    global token
    if token == "if":
        token = sigToken()
        if token == "(":
            token = sigToken()
            es_condicion()
            if token == ")":
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
        if token == "(":
            token = sigToken()
            es_expresion()
            if token == ")":
                token = sigToken()
                es_bloque()

def es_condicional_else():
    global token
    if token == "else":
        token = sigToken()
        es_bloque()

def es_bloque():
    global token
    if token == "{":
        token = sigToken()
        while (token != "}" and 
               token in ["tipo_Variable", "variable", "comentario_linea", 
                         "comentario_bloque", "for", "while", "if", 
                         "function", "llamada_funcion"]):
            es_sentencia()
        if token == "}":
            token = sigToken()

def es_expresion():
    global token
    es_termino()
    es_expresionprime()

def es_expresionprime():
    global token
    while token in ["suma", "resta"]:
        if token == "suma":
            es_suma()
        elif token == "resta":
            es_resta()
        es_termino()

def es_termino():
    global token
    es_factor()
    es_terminoprime()

def es_terminoprime():
    global token
    while token in ["multiplicacion", "division"]:
        if token == "multiplicacion":
            es_multiplicacion()
        elif token == "division":
            es_division()
        es_factor()

def es_factor():
    global token
    if token == "variable":
        es_variable()
    elif token == "entero":
        es_entero()
    elif token == "real":
        es_real()
    elif token in ["flotante", "flotante_negativo"]:
        es_flotante()

def es_flotante():
    global token
    if token in ["flotante", "flotante_negativo"]:
        token = sigToken()

def es_variable():
    global token
    if token == "variable":
        token = sigToken()

def es_asignacion():
    global token
    if token == "asignacion":
        token = sigToken()

def es_entero():
    global token
    if token == "entero":
        token = sigToken()

def es_real():
    global token
    if token == "real":
        token = sigToken()

def es_tipo_variable():
    global token
    if token == "tipo_Variable":
        token = sigToken()

def es_Semicolon():
    global token
    if token == "semicolon":
        token = sigToken()

def es_Comentario_linea():
    global token
    if token == "comentario_linea":
        token = sigToken()

def es_terminoprime():
    global token
    while token in ["multiplicacion", "division"]:
        if token == "multiplicacion":
            es_multiplicacion()
        elif token == "division":
            es_division()
        es_factor()

def es_factor():
    global token
    if token == "variable":
        es_variable()
    elif token == "entero":
        es_entero()
    elif token == "real":
        es_real()
    elif token in ["flotante", "flotante_negativo"]:
        es_flotante()

def es_flotante():
    global token
    if token in ["flotante", "flotante_negativo"]:
        token = sigToken()

def es_variable():
    global token
    if token == "variable":
        token = sigToken()

def es_asignacion():
    global token
    if token == "asignacion":
        token = sigToken()

def es_entero():
    global token
    if token == "entero":
        token = sigToken()

def es_real():
    global token
    if token == "real":
        token = sigToken()

def es_tipo_variable():
    global token
    if token == "tipo_Variable":
        token = sigToken()

def es_Semicolon():
    global token
    if token == "semicolon":
        token = sigToken()

def es_Comentario_linea():
    global token
    if token == "comentario_linea":
        token = sigToken()

def es_multiplicacion():
    global token
    if token == "multiplicacion":
        token = sigToken()


i = -1
token = sigToken()
es_sentencia()
if token == "":
    print("OKS\n")
else:
    showError(last, token)
    print("NOPE\n")
