"""
Este archivo obtiene los tokens del analizador lexico y  
realiza el analisis sintatico, esto comprobara que las
instrucciones sean factibles.

"""

from AnalizadorLexico import tokens

# Lenguaje a utilizar
"""
programa -> sentencia | comentario | funcion
funcion -> FUNCTION CORCHETE_INICIAL sentencia CORCHETE_FINAL
sentencia -> (sentencia | declaracion | asignacion) | declaracion | asignacion
declaracion -> tipo_variable variable
asignacion -> variable asignacion_expresion
asignacion_expresion -> expresion | expresion operador asignacion_expresion
expresion -> termino | termino operador expresion
termino -> variable | constante
variable -> VARIABLE
constante -> ENTERO | FLOTANTE | FLOTANTE_NEGATIVO | REAL
tipo_variable -> TIPO_VARIABLE
operador -> SUMA | RESTA | MULTIPLICACION | DIVISION
comentario -> COMENTARIO_LINEA | COMENTARIO_BLOQUE


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

<Semicolon> -> "Semicolon"

<Comentario_linea> -> "Comentario_linea"

<Comentario_bloque> -> "Comentario_bloque"

<Operador_Condicional> -> "Operador_cond"

<Operador_Logico> -> "Operador_log"

"""

Int = a + b/c;

a = 23;

// Comentario_bloque

for ( a; a < b ; Int b = s++){}

while (a > b){}

if(a>b){}
elseif(b<a){}
elseif(){}
else{}