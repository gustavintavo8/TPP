(require racket/function mzlib/compat)

;;; Definición de funciones recursivas sobre listas
;;; -----------------------------------------------
;;;
;;; En el análisis por casos se utilizará la notación funcional estándar:
;;; <nombre_función>(arg0, arg1, ...). Por ejemplo, para f(l):
;;;
;;; 1. Base       : el resultado de la función la lista vacía;
;;;                 f(()) ::= lo que corresponda según problema a resolver
;;; 2. Recurrencia: l no es la lista vacía; es decir, l=cons(car(l), cdr(l))
;;;      Hipótesis: se supone conocido f(cdr(l))=H
;;;          Tesis: obtener f(l) a partir de la hipótesis H en combinación
;;;                 con el elemento car(l) de la lista l que no forma parte
;;;                 del argumento de la hipótesis
;;;                 f(l) ::= combinar adecuadamente car(l) y H
;;;

;------------------------------------------------------------------------------------------------------------

;;Ejercicio 1

(define (toOneList s-expr)
  (cond
    ; Caso Base: Si la lista está vacía, devolvemos una lista vacía
    ((null? s-expr) '())
    ; Caso Recursivo: Si el primer elemento de s-expr es una lista,
    ;                 llamamos recursivamente a toOneList con el primer elemento y el resto de la lista,
    ;                 y concatenamos el resultado con la llamada recursiva a toOneList en el resto de s-expr
    ((list? (car s-expr)) (append (toOneList (car s-expr)) (toOneList (cdr s-expr))))
    ;                 Si el primer elemento de s-expr no es una lista, lo agregamos al resultado
    ;                 de llamar recursivamente a toOneList en el resto de s-expr
    (else (cons (car s-expr) (toOneList (cdr s-expr))))))


(displayln "ejercicio 1")
(toOneList '((1 2) (3 (4 5)) 6))
(toOneList '((())(3 (4 ((8)) 5)) (6 "a" 6)))
(toOneList '( (("H" #t -inf.0) () ((5)) ("e" #f pi)) "e"))
(toOneList '())
(toOneList '(()()()))

;------------------------------------------------------------------------------------------------------------

;;Ejercicio 2

(define (my-apply list aggregation seed)
  (cond
    ; Caso Base: Si la lista está vacía, retornamos el valor inicial (seed)
    ((null? list) seed)
    ; Caso Recursivo: Aplicamos la función de agregación al primer elemento de la lista
    ;                 y al resultado de llamar recursivamente a my-apply en el resto de la lista
    (else (aggregation (car list) (my-apply (cdr list) aggregation seed)))))

(displayln "ejercicio 2")
  (my-apply '(1 2 3 4) + 20)
  (my-apply '(1 2 3 4) max -inf.0)
  (my-apply '("HELLOW" " WORLD" "!!") string-append "")
(my-apply '() 'ANYTHING 'seed)

;------------------------------------------------------------------------------------------------------------

;;Ejercicio 2B

(define (my-apply1 list aggregation transformer seed)
  (cond
    ; Caso Base: Si la lista está vacía, retornamos el valor inicial (seed)
    ((null? list) seed)
    ; Caso Recursivo: Aplicamos la función de transformación al primer elemento de la lista,
    ;                 luego aplicamos la función de agregación al resultado de la transformación
    ;                 y el resultado de llamar recursivamente a my-applyl en el resto de la lista
    (else (my-apply1 (cdr list) aggregation transformer (aggregation (transformer (car list)) seed)))))


(displayln "ejercicio 2 A")
(my-apply1 '("hola" "hi" "hey") max string-length -inf.0)
(my-apply1 '("a" "b" "a" "c" "d" "a") + (lambda(x) (if [equal? "a" x] 1 0)) 0)

;------------------------------------------------------------------------------------------------------------

;;Ejercicio 3

;3a
(displayln "Ejercicio 3A")


(define vectors '((1 2 2 #t) (1 1 'a 1 1) ("hey" 2) (0 0 0 0 )))
(displayln "Ejercicio 3b")



  ;3c
 (define vectors '((1 2 2) (1 1 1 1 ) (0 0 0 0 0 1) (0) (2) (0 0) (0 0 0 0)))
 (displayln "Ejercicio 3c")


;4A
 (displayln "4A")

 (define vectors '((1 2 2 ) (1 1 1 1 ) (2) (0 0 0 0)))
 
 
 ;con curry
 (displayln "curry a la funcion piotencia")
 
  ;compose
  
  ;
 ;4B
(define vectors '((1 2 2 #t) (1 1 'a 1 1) ("hey" 2) (0 0 0 0 )))
(displayln "4B")

;sin funcion auxiliar

