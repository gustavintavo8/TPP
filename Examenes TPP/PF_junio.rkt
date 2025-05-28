(require mzlib/compat racket/function)

;; -----------
;; Ejercicio 1
;; -----------

; Función recursiva dada
; ----------------------
; add-nth(Sexpr, n, L) que retorna la lista resultante de añadir Sexpr en la posición n de L
; Se considera que 0 <= n <= length(L).

(define (add-nth Sexpr n L)
  (cond [(zero? n)
         (cons Sexpr L)]
        [else
         (cons (car L)
               (add-nth Sexpr (- n 1)  (cdr L)))]))


; RESUELVE A CONTINUACIÓN EL EJERCICIO 1
(define (add-nth . resto)
  (letrec ([f (lambda (Sexpr n L)
                (cond [(zero? n)
                       (cons Sexpr L)]
                      [else
                       (cons (car L)
                             (f Sexpr (- n 1)  (cdr L)))]))])
    (cond [(not (= (length resto) 3))
           (error "La funcion add-nth debe tener 3 argumentos (Sexpr, n y L)")]
          [(not (list (car resto)))
           (error "El primer argumento debe de ser una lista")]
          [(not (list (caddr resto)))
           (error "El tercer argumento debe de ser una lista")]
          [(not (>= (cadr resto) 0))
           (error "El valor de n debe de ser mayor o igual que 0")]
          [(> (cadr resto) (length (caddr resto)))
           (error "El valor de n debe ser menor o igual que la longitud de la lista L")]
          [else (f (car resto) (cadr resto) (caddr resto))])))
           
          
    

; COMPROBACIÓN DE FUNCIONAMIENTO
(displayln "Ejercicio 1, add-nth:")
;(add-nth '((a) b) -1 '(c d e))   ;--> ERROR
;(add-nth '((a) b) 4 '(c d e))   ;--> ERROR
; OTROS ERRORES QUE HAYA QUE COMPROBAR...
;
; ESTE NO DEBE DAR ERROR
(add-nth '((a) b) 0 '(c d e))   ; --> (((a) b) c d e)



;; -----------
;; Ejercicio 2
;; -----------

; INDICA A CONTINUACIÓN EL ANALISIS POR CASOS


; ESCRIBE DEBAJO LA IMPLEMENTACIÓN
;(define (filter2 f-xy l1 l2)
;  (cond [(null? l1) '()]
;        [else (if (f-xy (car l1) (car l2))) (cons (car l1) (car l2))


; COMPROBACIÓN DE FUNCIONAMIENTO
;(displayln "\nEjercicio 2, filter2:")
;(filter2 < '(-1 3 0) '(4 1 2))   ; => ((-1 4) (0 2))
;(filter2 [lambda(x y) (and (number? x) (number? y))] '(1 a 2 b 3) '(a 0 -1 2 5))  ; =>((2 -1) (3 5))

;; ------------------
;; Ejercicios de FOS
;; ------------------

; El símbolo 'Nadal_RG' muestra información sobre los 14 torneos de Roland Garros que ha ganado
; Rafael Nadal. Cada torneo ganado es un registro en forma de lista, y cada una guarda, de
; izquierda a derecha: el año de celebración del torneo, el puesto del ranking ATP que ocupaba
; Nadal aquel año, el número de sets que jugó en total durante el torneo y el número de sets
; que se jugaron en la final.

(define Nadal_RG '((2005 4 24 4)
                   (2006 2 23 4)
                   (2007 2 22 4)
                   (2008 2 21 3)
                   (2010 2 21 3)
                   (2011 1 24 4)
                   (2012 2 22 4)
                   (2013 3 25 3)
                   (2014 1 23 4)
                   (2017 4 20 3)
                   (2018 1 22 3)
                   (2019 2 23 4)
                   (2020 2 21 3)
                   (2022 5 23 3)))


;; -----------
;; Ejercicio 3 - Función stats_anio(Datos, anio)
;; -----------

; ESCRIBE DEBAJO LA IMPLEMENTACIÓN
;(define (stats_anio datos anio)
 ; (car (filter (lambda(x) (equal? anio (car x))) datos)))

(define (stats_anio datos anio)
  (car (filter (compose (curry equal? anio) car) datos)))

; COMPROBACIÓN DE FUNCIONAMIENTO
(displayln "\nEjercicio 3, stats_anio:")
(stats_anio Nadal_RG 2008)    ; => (2008 2 21 3)
(stats_anio Nadal_RG 2014)    ; => (2014 1 23 4)
(stats_anio Nadal_RG 2022)    ; => (2022 5 23 3)


;; -----------
;; Ejercicio 4 -  Función edad(Datos, nac)
;; -----------

; ESCRIBE DEBAJO LA IMPLEMENTACIÓN
(define (edad datos nac)
  (map (lambda(x) (append x (list (- (car x) nac)))) datos))
  
; COMPROBACIÓN DE FUNCIONAMIENTO
(displayln "\nEjercicio 4, edad:")
(edad Nadal_RG 1986)    ; => ((2005 4 24 4 19) (2006 2 23 4 20) (2007 2 22 4 21) ...)


;; -----------
;; Ejercicio 5 - Función media(stat, Datos)
;; -----------

;; Las siguientes funciones reciben un registro de la lista de datos y devuelven,
;; respectivamente, el ranking ATP, el número total de sets jugados durante el
;; torneo y el número de sets de la final.

(define (ranking torneo)
  (cadr torneo))   ; (ranking (2006 2 23 4)) => 2

(define (total_sets torneo)
  (caddr torneo))   ; (total_sets (2006 2 23 4)) => 23

(define (sets_final torneo)
  (cadddr torneo))   ; (sets_final (2006 2 23 4)) => 4


; ESCRIBE DEBAJO LA IMPLEMENTACIÓN
(define (media datos stat)
  (exact->inexact (/ (apply + (map stat datos)) (length (map stat datos)))))
  

; COMPROBACIÓN DE FUNCIONAMIENTO
(displayln "\nEjercicio 5, media:")
(media Nadal_RG ranking)    ; => 2.3571...
(media Nadal_RG total_sets)    ; => 22.42847...
(media Nadal_RG sets_final)    ; => 3.5


