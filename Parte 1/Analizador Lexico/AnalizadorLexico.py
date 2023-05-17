"""
Este archivo recorre el archivo dado y utiliza expresiones 
regulares de un lexema para encontrar matches y asignar tokens 
a esos matches.
"""

import re

# Creamos el lexema a usar
# Meta characters de regex \b(inicio, fin de palabra), \w(cualquier caracter de [A-Za-z0-9]), \s(cualquier espacio en blanco)
comentario_bloque = r'\/\*[\s\S]*?\*\/'
comentario_linea = r'\/\/[^\n]*'
string = r'\".*?\"'
tipo_Variable = r'\b(?:[I|i]nt|[F|f]loat|[S|s]tring)\b'
identificador = r'\b[a-zA-Z]\w*\b'
asignacion = r'='
parentesis_apertura = r'\('
parentesis_cierre = r'\)'
entero = r'(?:[+-]\s*)?\d+'
flotante = r'[+]?\s*\d+\s*\.\s*\d+\s*'
flotante_negativo = r'\-\s*\d+\s*\.\s*\d+' 
real = r'[+-]?\s*\d+\s*\.\s*\d+\s*[eE]\s*[+-]?\s*\d+' 
resta = r'\-'
multiplicacion = r'\*'
division = r'\/'
potencia = r'\^'
semicolon = r'\;'
operador_logico = r'(?:<=|>=|!=|==|<|>)'
operador_condicional = r'(?:[A|a]nd|[O|o]r|[N|n]ot)'

# Creamos un string de todos los patrones, el orden del patron es importante!
regex = '|'.join([ string, comentario_bloque, comentario_linea, tipo_Variable, identificador, asignacion, parentesis_apertura, parentesis_cierre, real, flotante, flotante_negativo, entero, resta, multiplicacion, division, potencia, semicolon ])
print("Inicio: " + regex)
# Juntamos todos los patrones en un solo regex
pattern = re.compile(regex,re.DOTALL)

tokens = [] # Lista de tokens encontrados
with open('entrada.txt') as entrada, open('salida.txt', 'w') as salida:                                         # Abrimos el archivo entrada, se cierra automático con with
    #for line in entrada:                                                    # Recorremos todas las lineas del archivo
    contenido = entrada.read()
    matches = pattern.findall(contenido)                                      # Encontramos todos los tokens de la linea
    # print(matches) # Para checar todos los tokens por linea
    tokens.extend([                                                      # Juntamos todos los tokens a su patron por linea en una lista
        (match, 'string') if re.match(string, match) else
        (match, 'comentario_bloque') if re.match(comentario_bloque, match) else
        (match, 'comentario_linea') if re.match(comentario_linea, match) else
        (match, 'tipo_Variable') if re.match(tipo_Variable, match) else
        (match, 'variable') if re.match(identificador, match) else
        (match, 'parentesis_apertura') if re.match(parentesis_apertura, match) else
        (match, 'parentesis_cierre') if re.match(parentesis_cierre, match) else
        (match, 'asignacion') if re.match(asignacion, match) else
        (match, 'real') if re.match(real, match) else
        (match, 'flotante') if re.match(flotante, match) else
        (match, 'flotante_negativo') if re.match(flotante_negativo, match) else
        (match, 'entero') if re.match(entero, match) else
        (match, 'resta') if re.match(resta, match) else
        (match, 'multiplicacion') if re.match(multiplicacion, match) else
        (match, 'division') if re.match(division, match) else
        (match, 'potencia') if re.match(potencia, match) else
        (match, 'semicolon') if re.match(semicolon, match) else
        None
        for match in matches                                              # Recorremos cada token de la linea (match = token, matches = tokens de linea)
    ])
    print(tokens, file=salida)                                                      # Se imprime cada parte
