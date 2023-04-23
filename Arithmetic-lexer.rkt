#|
Arithmetic lexer activity

 This code is based on 02_token_list.rkt by gilecheverria
 Thanks to gilecheverria team for providing such a helpful file!

Carlos Fragoso - A01028113

|#

#lang racket

(require racket/trace)

(provide dfa arithmetic-lexer delta-arithmetic)

; Define a stucture that describes a DFA
(struct dfa (transition initial accept))

(define (arithmetic-lexer strng)
  " Evaluate an arithmetic expression "
  (validate-dfa strng (dfa delta-arithmetic 'start '(int float exp id close_par comment))))

(define (printresults lst)
  (cond ((null? lst)
         (newline)
        'done)
      (else
       (display (car(car lst)))
       (display "   |   ")
       (display (car(cdr(car lst))))
       (newline)
       (printresults (cdr lst)))))
      

; Function to evaluate a string using a DFA
; Receives the string to test and a structure for a DFA
(define (validate-dfa input dfa-def)
  (let loop
    ; Convert the string into a list of chars
    ([char-list (string->list input)]
     ; Get the initial state from the DFA definition
     [state (dfa-initial dfa-def)]
     ; The list to be returned with all the types of tokens found
     [values '()]
     [tokens '()])
    (cond
      ; Check if the final state is in the list of acceptable states
      [(empty? char-list)
       (if (member state (dfa-accept dfa-def))
         (cond ((eq? state 'spa) 
                (  (display "Value   |")
                   (display "   Token")
                   (newline)
                   (printresults (reverse tokens)))) ; 
                (else
                     (display "Value   |")
                     (display "   Token")
                     (newline)
                     (printresults (reverse (cons (list (list->string (reverse values)) state) tokens))))
                )
         'invalid)]
      [else
        ; Extract the two values returned by the transition function
        (let-values
          ;   var1      var2   Call to the transition function
          ([(new-state found) ((dfa-transition dfa-def) state (car char-list))])
          (loop
            ; New list
            (cdr char-list)
            ; New state, obtained by calling the transition function
            new-state
            (if (not (char-whitespace? (car char-list)))
                (if found
                    (cons (car char-list) '()) 
                    (cons (car char-list) values))
                (if (eq? state 'comment)
                    (cons (car char-list) values)
                    '()))
            (if found
                (cons (list (list->string (reverse values)) found) tokens)                                
                 tokens)))])))

(define (char-operator? char)
  " Check if the character is considered an operator "
  (member char (string->list "=+-*/")))

; Accept numbers of different types
; Star state: 'start
; Accept states: 'int 'float
; The function returns pairs of values: the new state, and the token that had been found
; if the token has not fihised yet, the type is false ( #f )
(define (delta-arithmetic state char)
  (cond
    [(eq? state 'start) (cond
       [(char-numeric? char) (values 'int #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)]
       [(char-alphabetic? char) (values 'id #f)]
       [(eq? char #\_) (values 'id #f)]
       [(eq? char #\/) (values 'comment #f)]
       [(eq? char #\() (values 'open #f)]
       [else (values 'inv #f)])]
    [(eq? state 'comment) (values 'comment #f)]
    [(eq? state 'sign) (cond
       [(char-numeric? char) (values 'int #f)]
       [else (values 'inv #f)])]
    [(eq? state 'int) (cond
       [(char-numeric? char) (values 'int #f)]
       [(eq? char #\/) (values 'comment #f)]
       [(eq? char #\.) (values 'dot #f)]
       [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
       [(char-operator? char) (values 'op 'int)]
       [(char-whitespace? char) (values 'spa 'int)]
       [else (values 'inv #f)])]
    [(eq? state 'dot) (cond
       [(char-numeric? char) (values 'float #f)]
       [else (values 'inv #f)])]
    [(eq? state 'float) (cond
       [(char-numeric? char) (values 'float #f)]
       [(eq? char #\/) (values 'comment #f)]
       [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
       [(char-operator? char) (values 'op 'float)]
       [(char-whitespace? char) (values 'spa 'float)]
       [else (values 'inv #f)])]
    [(eq? state 'e) (cond
       [(char-numeric? char) (values 'exp #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'e_sign #f)]
       [else (values 'inv #f)])]
    [(eq? state 'e_sign) (cond
       [(char-numeric? char) (values 'exp #f)]
       [else (values 'inv #f)])]
    [(eq? state 'exp) (cond
       [(char-numeric? char) (values 'exp #f)]
       [(char-operator? char) (values 'op 'exp)]
       [(char-whitespace? char) (values 'spa 'exp)]
       [else (values 'inv #f)])]
    [(eq? state 'id) (cond
       [(char-numeric? char) (values 'id #f)]
       [(eq? char #\/) (values 'comment #f)]
       [(char-alphabetic? char) (values 'id #f)]
       [(eq? char #\_) (values 'id #f)]
       [(char-operator? char) (values 'op 'id)]
       [(char-whitespace? char) (values 'spa 'id)]
       [else (values 'inv #f)])]
    [(eq? state 'op) (cond
       [(char-numeric? char) (values 'int 'op)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign 'op)]
       [(char-alphabetic? char) (values 'id 'op)]
       [(char-whitespace? char) (values 'op_spa 'op)]
       [else (values 'inv #f)])]
    [(eq? state 'spa) (cond
       [(eq? char #\/) (values 'comment #f)]
       [(char-operator? char) (values 'op #f)]
       [(char-whitespace? char) (values 'spa #f)]
       [else (values 'inv #f)])]
    [(eq? state 'op_spa) (cond
       [(char-numeric? char) (values 'int #f)]
       [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)]
       [(char-alphabetic? char) (values 'id #f)]
       [(eq? char #\_) (values 'id #f)]
       [(char-whitespace? char) (values 'op_spa #f)]
       [else (values 'inv #f)])]
    [(eq? state 'open) (cond
       [(char-numeric? char) (values 'int #f)]
       [(eq? char #\)) (values 'close #f)]
       [(char-whitespace? char) (values 'spa 'int)]
       [else (values 'inv #f)])]
    [(eq? state 'close) (cond
       [(char-numeric? char) (values 'int #f)]
       [(char-whitespace? char) (values 'spa 'int)]
       [else (values 'inv #f)])]
    ))