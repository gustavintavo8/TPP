(require mzlib/compat racket/function)    ;  para incluir atom? y curry

; EJERCICIO 1
; Función quitarNegativos(Sexpr)
; caso base : quitarNegativos(()): ()
;             quitarNegativos(atomo): si atomo>= 0 entoves atomo
;                                         sino 0
;                                     finsi
; Recurrencia: Sexpr= (cons (car Sexpr) (cdr Sexpr))
;Hipotesis: se conoce H1=quitarNegativos((car Sexpr))
;              y      H2=quitarnegativos(cdr Sexpr)
;Tesis: quitarNegativos(Sexpr)=(cons H1 H2)
(define (quitarNegativos Sexpr)
 (cond ( (null? Sexpr) '())
       ( (atom? Sexpr) (if (< Sexpr 0) 0 Sexpr))
       (else (cons (quitarNegativos (car Sexpr)) (quitarNegativos (cdr Sexpr))))))

(display "  1  => ")
(quitarNegativos '((2.4 -3 (1 -4)) (3.5 7) -5.25)) ;=> ((2.4 0 (1 0)) (3.5 7) 0)


; EJERCICIO 2
; Función comparar1(a, b)

(define (compare x y)
  (cond ([< x y] -1)
        ([= x y] 0)
        (else 1)))
;Caso base: comparar1(()())= ()
;Recurrencia: l1=(cons (car l1) (cdr l1)) y l2=(cons (car l2) (cdr l2))
;Hipotesis: se supone H=comparar1((cdr l1) (cdr l2))
; tesis: (cons (compare (car l1) (car l2)) H)
(define (comparar1 l1 l2)
(cond ( (null? l1) ())
      (else (cons (compare (car l1) (car l2)) (comparar1 (cdr l1) (cdr l2))))))

(display "  2  => ")
(comparar1 '(2.4 7 1 -4) '(3 7 -5.25 0)) ;=> (-1 0 1 -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definición del símbolo Datos para utilizar en los ejercicios 3 y 4 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Datos '((5 0 3 4) (18 28 -6 90 -24 -2 10) (-3 12 0 10 -18 1 2 3 -3 7 16) (2 -6)))

; EJERCICIO 3
; Filtros en Datos

; 1. Obtener todas las sublistas de Datos que contengan el valor 0

(display "3.1  sin curry => ")
(filter (lambda(x) (member 0 x)) Datos)
(display "3.1  con curry => ")
(filter (curry member 0) Datos)

; 2. Obtener todas las sublistas de Datos que tengan cuatro o menos números

(display "3.2  => ")
(filter (lambda (x) (<= (length x) 4)) Datos)

; 3. Obtener todas las sublistas de Datos cuya suma no sea negativa

(display "3.3  => ")
(filter (lambda (x) (>= (apply + x) 0)) Datos)

; 4. Obtener todas las sublistas de Datos que únicamente contengan valores no negativos

(display "3.4 sin curry => ")
(filter (lambda (x) (null? (filter (lambda (n) (> 0 n)) x ))) Datos)

(display "3.4  con curry => ")
(filter (lambda (x) (null? (filter (curry > 0) x))) Datos)
;otra forma
(display "3.4 con el apply => ")
(filter (lambda(x) (>= (apply min x) 0)) Datos)
; EJERCICIO 4
; Utilizar FOS en las expresiones solicitadas

; 1. Expresión que retorna una lista igual que Datos, pero sin los números impares
;    contenidos en las sublistas

(display "4.1  sin curry => ");
(map (lambda(l) (filter even? l)) Datos) 
(display "4.1 con curry => ");
(map (curry filter even?) Datos)

; 2. Expresión que retorna una lista análoga a Datos,  con la única diferencia de que
;    todas las sublistas deben incluir un nuevo número al final, por ejemplo, el 100

(display "4.2  => ")
(map (lambda(l) (append l (list 100))) Datos)

; 3. Expresión que permita obtener una lista análoga a Datos, pero en la que se
;    reemplacen todos los números negativos de las sublitas por ceros

(display "4.3 sin curry  => ")
(map (lambda(l) (map (lambda (n) (if (> 0 n) 0 n)) l)) Datos)
(display "4.3 con curry => ")
(map (curry map (lambda (n) (if (> 0 n) 0 n))) Datos)

; 4. Expresión que permite obtener en una única lista todos los números que contiene
;    Datos

(display "4.4  => ")
(apply append Datos)

; EJERCICIO 5
; Función comparar2(a, b) validando sus argumentos
(define comparar2
  (lambda lista
    (let ( (f (lambda ( l1 l2)
                   (map compare l1 l2))))
      ;body
      (cond ((not (equal? (length lista) 2)) (error "tiene que haber dos argumentos"))
            ((or (not (list? (car lista))) (not (list? (cadr lista)))) (error "los dos argumentos tienen que ser listas"))
            ((not (equal? (length (car lista)) (length (cadr lista)))) (error "Las listas no tienen la misma longitug"))
            ((not (null? (filter (lambda(x) (not (number? x))) (car lista)))) (error "la primera lista tiene elementos que no son numeros"))
            ((not (null? (filter (lambda(x) (not (number? x))) (cadr lista)))) (error "la segunda lista tiene elementos que no son numeros"))
            (else (f (car lista) (cadr lista)))))))
 
(display "  5  => ")
(comparar2 '(2.4 7 1 -4) '(3 7 -5.25 0)) ;=> (-1 0 1 -1)
(display "  5  => ")
(comparar2 '(2 0 1) '(2 b 3)) ;=> error

