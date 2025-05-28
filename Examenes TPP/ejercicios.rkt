(require mzlib/compat)
;Ejercicio 1
; Caso base: add(x,(),f)-->x
;Recurrencia: l=(car l) y (cdr l)
;Hipótesis: H=add(x,(cdr l),f)
; Tesis: si (f(x,(car l)) entonces (cons (x l))
;         sino (cons (car l) H)
;         finsi

(define (add x l f)
 (cond ( (null? l) (list x))
       ( (f x (car l)) (cons x l))
       (else (cons (car l) (add x (cdr l) f)))))

(displayln "funcion add")
(add 5 '(10 8 7 3 1) >)
(add 15 '(10 8 7 3 1) >)
(add 0 '(10 8 7 3 1) >)
(add 5 '( 1 3 6 7 8) <)

;Ejercicio 2
(define (add-length l ll)
  (add l ll (lambda (a b) (< (length a) (length b)))))
(displayln "add-length")
(add-length '(a b) '((a) (1) (d g) (x y (z))))

;Ejercicio 3
;caso base : addlast (x ())-> ()
;Recurrencia: l=(car l)y (cdr l)
;Hipotesis: H=addLast(x (cdr l))
; Tesis: si l no es vacia
;            (cons (append(car l) x) (addlast(x (cdr l))
;        finsi

(define addLast
  (lambda lista
    (letrec( (funcionR (lambda (valor listaR)
             (cond ((null? listaR) listaR)
             (else (cons (append (car listaR) (list valor)) (funcionR valor (cdr listaR)))))
              )))
      ;body
      (funcionR (car lista) (cdr lista)))))
(displayln "addLast")
(addLast '(1) '(a b) '((d) e f) '())
;Ejercicio4
(define addLast+
  (lambda lista
    (letrec( (funcionR (lambda (valor listaR)
             (cond ((null? listaR) listaR)
             (else (cons (append (car listaR) (list valor)) (funcionR valor (cdr listaR)))))
              )))
      ;body
      (cond ( (< (length lista) 2) (error "Argumentos erroneos, se necesita un valor y como minimo una lista"))
            ( (not (equal? (length (cdr lista)) (length (filter list? (cdr lista))))) (error "algun elemento no es una lista"))
      (else (funcionR (car lista) (cdr lista)))))))

(displayln "addLast+")
(addLast+ '(1) '(a b) '((d) e f) '())
;(addLast+ '(1) 'a '((d) e f) '())
;(addLast+ '(1))
(define (enteros x y)
  (if (>= x y) ()
      (cons x (enteros (+ x 1) y))))
(enteros -2 4); => (-2 -1 0 1 2 3)

;Ejercicio 5
(define (suc0 a b)
  ( map (lambda (x) (* a x))    (enteros 0 b)))
  (displayln "suc0")
  (suc0 3 6)

 ;Ejercicio 6
(define (sucesion x r n)
  (map (lambda (z) (+ z x)) (suc0 r n)))
(displayln "sucesion")
(sucesion -5 3 6)
(define (serie x r n)
  (apply + (sucesion x r n)))
(displayln "serie")
(serie -5 3 6)

(define UNO
 '((ana ((4 rojo) (9 rojo) (9 verde) (comodin-color) (9 verde) (8 verde) (3 azul)))
 (rosa ((0 amarillo) (1 amarillo) (3 rojo) (8 azul) (4 verde) (3 rojo) (9 azul)))
 (luis ((comodin-color) (8 amarillo) (5 rojo) (7 amarillo) (5 verde) (4 amarillo)))
 (pedro ((7 amarillo) (1 rojo) (4 verde) (9 rojo) (7 rojo) (6 amarillo) (4 rojo)))
 (maria ((7 verde) (8 amarillo) (0 rojo) (comodin-roba4) (3 verde) (8 verde) (3 azul)))
 (carmen ((8 azul) (5 verde) (5 amarillo) (6 amarillo) (3 amarillo) (6 azul) (roba2 verde)))
 (blanca ((3 amarillo) (1 rojo) (5 amarillo) (4 amarillo) (2 azul) (9 azul) (7 azul)))
 (quique ((9 amarillo) (reversa rojo) (2 rojo) (6 verde) (8 rojo) (1 azul) (1 verde)))))
(define (nombrePersona dato)
  (car dato))
;(nombre '(ana ((4 rojo) (9 rojo) (9 verde) (comodin-color) (9 verde) (8 verde) (3 azul))))
;Ejercicio 7.1
(define (jugador  nombre Datos)
  ( car (filter (lambda (persona) (equal? nombre (nombrePersona persona))) Datos)))

 (displayln "jugador")
 (jugador 'rosa UNO)
 ;Ejercicio 7.2
 (define (accion? cartaAccion Jugador)
  ( not (null? (filter (lambda (x) (member cartaAccion x)) (cadr Jugador)))))
 (displayln "accion?")
 (accion? 'reversa (jugador 'quique UNO)); => #t
 (accion? 'comodin-color (cadr UNO)); => #f
;ejercicio 7.3
 (define (accion-x? Jugador)
  ( not (null? (filter (lambda (x) (accion? x Jugador)) '(roba2 reversa salta comodin-color comodin-roba4)))))

(displayln "accion-x?")
(accion-x? (jugador 'pedro UNO)); => #f
(accion-x? (car UNO)); => #t

;ejercicio 7.4
(define (cartas-valor valor jugador)
( filter (lambda(x) (member valor x)) (cadr jugador)))

(displayln "cartas-valor")
(cartas-valor 'reversa (jugador 'quique UNO)); => ((reversa rojo))
(cartas-valor 3 (cadr UNO)); => ((3 rojo) (3 rojo))
(define (es-comodin? carta)
 (or (member 'comodin-color carta) (member 'comodin-roba4 carta)))
;ejercicio 7.5
(define (cartas-color color jugador)
( filter (lambda(x) (or (member color x) (es-comodin? x))) (cadr jugador)))
(displayln "cartas-color")
(cartas-color 'rojo (jugador 'maria UNO)); => ((0 rojo) (comodin-roba4))
(cartas-color 'azul (cadr UNO)); => ((8 azul) (9 azul))
;ejercicio 7.6
(define (compatibles carta dato)
 (cond (( es-comodin? carta) (cadr dato))
       (else ( append (cartas-valor (car carta) dato) (cartas-color (cadr carta) dato)))))


(displayln "compatibles")
(compatibles '(0 azul) (jugador 'maria UNO))

(compatibles '(9 amarillo) (jugador 'blanca UNO))

;ejercico 7.7
(define (jugador-con-comodin jugador)
  ( cond ( (not (null? (filter (lambda (carta) (es-comodin? carta)) (cadr jugador)))) (car jugador))
         (else '())))
(displayln "jugadorconcomodin")
(jugador-con-comodin (car UNO))

(define (jugadores-con-comodin datos)
( filter (lambda (x) (not (null? x))) (map (lambda(jugador) (jugador-con-comodin jugador)) datos)))

(displayln "jugadoresconcomodines")
(jugadores-con-comodin UNO)

;ejercicio 7.8
(define (jugadores-cartas-valor valor datos)
  (map (lambda(x) (cartas-valor valor x)) datos))
(displayln "jugadores-cartas-valor")
(jugadores-cartas-valor 3 UNO)
; ((ana ((3 azul)))
; (rosa ((3 rojo) (3 rojo)))
; (maria ((3 verde) (3 azul)))
; (carmen ((3 amarillo)))
; (blanca ((3 amarillo))))

;ejercicio 7.9
