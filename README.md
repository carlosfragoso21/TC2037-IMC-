## TC2037-IMC-
# Implementacion de metodos computacionales
Documentación Actividad 3.2 Programando un DFA

## Introduction
Aquí podrías dividir el contenido del documento en secciones, y describir cada una de ellas de manera breve.

## DFA design
![diagrama automata](./diseño_act.png)  

## user manual
The code is an implementation of a lexer, a program that reads a string of characters and recognizes the different types of tokens in the input.

## Requirements
Before running this program, you will need to have Racket installed on your computer. You can download it from the following link: https://download.racket-lang.org/

## How to use the program
1.-Open Racket on your computer.

2.-Open the file that contains the source code in Racket.

3.-Execute the source code to load all functions into the Racket environment.

4.-In the Racket console, call the arithmetic-lexer function with a character string as an argument.

For example:

(arithmetic-lexer "3.14 + 2 * (4 - 2)")

The arithmetic-lexer function returns a list of tokens found in the input string.
Each token is a list of two elements: the token value (e.g. the number 3.14) and its type (e.g. 'float').

The types of tokens that can be found are:

'int': for integer numbers.
'float': for floating-point numbers.
'id': for identifiers (e.g. variable names).
'open': for opening parentheses.
'close_par': for closing parentheses.
'op': for arithmetic operators (+, -, *, /, =).
'comment': for comments.
'spa': for white spaces.

## Output example
For the input "3.14 + 2 * (4 - 2)", the program would produce an output like this:

"Value     Token"
3.14       float
+          op
2          int
*          op
(          open
4          int
-          op
2          int
)          close_par

'done

## Credits
This code is based on 02_token_list.rkt by gilecheverria. We thank the gilecheverria team for providing such a useful file.
