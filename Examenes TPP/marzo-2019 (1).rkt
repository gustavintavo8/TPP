(require mzlib/compat racket/function)    ;  para incluir atom? y curry

; EJERCICIO 1
; Función quitarNegativos(Sexpr)


(display "  1  => ")
(quitarNegativos '((2.4 -3 (1 -4)) (3.5 7) -5.25)) ;=> ((2.4 0 (1 0)) (3.5 7) 0)


; EJERCICIO 2
; Función comparar1(a, b)

(define (compare x y)
  (cond ([< x y] -1)
        ([= x y] 0)
        (else 1)))


(display "  2  => ")
(comparar1 '(2.4 7 1 -4) '(3 7 -5.25 0)) ;=> (-1 0 1 -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definición del símbolo Datos para utilizar en los ejercicios 3 y 4 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Datos '((5 0 3 4) (18 28 -6 90 -24 -2 10) (-3 12 0 10 -18 1 2 3 -3 7 16) (2 -6)))

; EJERCICIO 3
; Filtros en Datos

; 1. Obtener todas las sublistas de Datos que únicamente contengan el valor 0

(display "3.1  sin curry => ")

(display "3.1  con curry => ")


; 2. Obtener todas las sublistas de Datos que tengan cuatro o menos números

(display "3.2  => ")


; 3. Obtener todas las sublistas de Datos cuya suma no sea negativa

(display "3.3  => ")


; 4. Obtener todas las sublistas de Datos que únicamente contengan valores no negativos

(display "3.4 sin curry => ")

(display "3.4  con curry => ")



; EJERCICIO 4
; Utilizar FOS en las expresiones solicitadas

; 1. Expresión que retorna una lista igual que Datos, pero sin los números impares
;    contenidos en las sublistas

(display "4.1  sin curry => ");

(display "4.1 con curry => ");


; 2. Expresión que retorna una lista análoga a Datos,  con la única diferencia de que
;    todas las sublistas deben incluir un nuevo número al final, por ejemplo, el 100

(display "4.2  => ")


; 3. Expresión que permita obtener una lista análoga a Datos, pero en la que se
;    reemplacen todos los números negativos de las sublitas por ceros

(display "4.3 sin curry  => ")

(display "4.3 con curry => ")


; 4. Expresión que permite obtener en una única lista todos los números que contiene
;    Datos

(display "4.4  => ")


; EJERCICIO 5
; Función comparar2(a, b) validando sus argumentos



(display "  5  => ")
(comparar2 '(2.4 7 1 -4) '(3 7 -5.25 0)) ;=> (-1 0 1 -1)
(display "  5  => ")
(comparar2 '(2 0 1) '(2 b 3)) ;=> error

