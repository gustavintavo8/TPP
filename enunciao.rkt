(require mzlib/compat racket/function)

; Name and surnames: Daniel López Gala.

;___________________________________________________________________________
;
;          NOTES IN RELATION TO THE ANSWERS OF THE EXERCISES
;
; 1. It is COMPULSORY to do the analysis of cases for every recursive function.
;
; 2. If it is possible to do the currying in a function with HOF, not doing it
;    will be taken into account negatively.
;
; 3. The efficiency of the programs will be taken into account positively.
;
; 4. You must save the file with your name and surnames, and upload it as .rkt to the CV.
;___________________________________________________________________________


;; Exercise 1 (2,5 points) - Function substract(l1, l2):
;;                Given two lists l1 and l2, returns the list l1 after removing the elements
;;                which also belong to l2.
;;                The function must be recursive, and therefore the case analysis must be done.
;;                Make substract have a variable number of arguments but check that it only
;;                receives two and both are lists.
;;

;;; Definición de funciones recursivas sobre listas
;;; -----------------------------------------------
;;;
;;; En el análisis por casos se utilizará la notación funcional estándar:
;;;  substract(l1, l2, ...):
;;;
;;; 1. Base       : l1 es la lista vacia -> substract((),l2) = ()
;;; 2. Recurrencia: l1 no es la lista vacía; es decir, l1=cons(car(l1), cdr(l1))
;;;      Hipótesis: se supone conocido substract(cdr(l1),l2)=H
;;;          Tesis: substract(l1,l2) = si car(l1) está en l2 entonces
;;;                                       H
;;;                                    si no
;;;                                       cons(car(l1),H)
;;;

(define substract
  (lambda rest
    (letrec ((f(lambda (l1 l2)
                 (cond [(null? l1) ()]
                       [(member (car l1) l2) (substract(cdr l1)l2)]
                       [else(cons(car l1)(substract(cdr l1)l2))]))))
      (cond [(not (= (length rest)2)) (error "substract(A, B): Debe recibir solo 2 listas")]
            [(not (list? (car rest)))(error "substract(A, B): A debe ser una lista")]
            [(not (list? (cadr rest)))(error "substract(A, B): B debe ser una lista")]
            [else (f (car rest)(cadr rest))]))))


(displayln "Exercise 1, substract:")
(substract '(4 8 5 6 4 12) '(3 11 8 9 4))    ; => (5 6 12)


;; Exercise 2 (1,5 points) - Function contains?(S-expr, x)
;;                Given an S-expression S and an atom x, returns true if x is contained in S and
;;                false if not.
;;                The function must be recursive, and therefore the case analysis must be done.
;;

;;; Definición de funciones recursivas sobre listas
;;; -----------------------------------------------
;;;
;;; En el análisis por casos se utilizará la notación funcional estándar:
;;;  contains?(S-expr, x):
;;;
;;; 1. Base       : S-expr es la lista vacia -> contains?((), x) = #f
;;;                 S-expr es un atomo ->  contains?(S-expr, x) = si S-expr = x entonces #t
;;;                                                               sino #f
;;; 2. Recurrencia: S-expr no es la lista vacía; es decir, S-expr=cons(car(S-expr), cdr(S-expr))
;;;      Hipótesis: se supone conocido contains?(car(S-expr), x) = H1 y contains?(cdr(S-expr), x) = H2
;;;          Tesis: substract(l1,l2) = or(H1,H2)


(define (contains? S-expr x)
  (cond [(null? S-expr) #f]
        [(atom? S-expr) (if (equal? S-expr x) #t #f)]
        [else(or (contains?(car S-expr)x)(contains?(cdr S-expr)x))]))


(displayln "Exercise 2, contains?:")
(contains? '(a (b ((c (d) (a))))) 'a)    ; => #t
(contains? '(a (b ((c (d) (a))))) 'd)    ; => #t
(contains? '(a (b ((c (d) (a))))) 'f)    ; => #f


;; 'Team_MADEC' stores information about the players of a basketball team.
;; Every register has the form (name (points assists rebounds))

(define Team_MADEC '((Marta (15 3 5))
               (Alvaro (21 7 2))
               (Daniel (11 4 8))
               (Eva (30 2 1))
               (Carlota (9 13 7))))

;; --------------------------------------------------
;; Solve with HOF the following exercises
;; --------------------------------------------------

;; Exercise 3 (1,5 points) - Functions points(name, team), assists(name, team) and rebounds(name, team)
;;              Given a name and a team like the one presented, define three functions points,
;;              assists and rebounds that return the asked information about the given player
;;

(define (points name team)
  (car(cadar (filter (curry member name) team))))

(define (assists name team)
  (cadr(cadar (filter (curry member name) team))))

(define (rebounds name team)
  (caddr(cadar (filter (curry member name) team))))


(displayln "Exercise 3, points, assists, rebounds:")
(points 'Alvaro Team_MADEC)    ; => 21
(assists 'Alvaro Team_MADEC)    ; => 7
(rebounds 'Alvaro Team_MADEC)    ; => 2


;; Exercise 4 (1.5 points) -  Function maxStat(name, team)
;;              Given a team and a player's name, returns the score he/she achieved for
;;              his/her best stat (points, assists or rebounds)
;;

(define (maxStat name team)
  (apply max (cadar (filter (curry member name) team))))


(displayln "Exercise 4, maxStat:")
(maxStat 'Marta Team_MADEC)    ; => 15
(maxStat 'Daniel Team_MADEC)    ; => 11
(maxStat 'Carlota Team_MADEC)    ; => 13


;; Exercise 5 (1.5 points) - Function double-double?(team)
;;              Given a team, returns true if there is a player that achieved a double double.
;;              In basketball, a double double is achieved when a player gets 10 or more in
;;              at least two stats. For example, 12 points and 10 assists.
;;

(define (double-double? team)
  (not (null? (filter (compose (curry <= 2) length) (map (compose (curry filter (curry <= 10)) cadr) team)))))

(displayln "Exercise 5, double-double?:")
(double-double? Team_MADEC)    ; => #f


;; Exercise 6 (1.5 points) - Function totalStat(stat, team)
;;              Given a team and a stat ('points', 'assists' or 'rebounds'), returns the
;;              total score of all the team's players for the given stat.
;;

(define (totalStat stat team)
  (apply + (map (lambda (x) (stat x team)) (map car team))))

(displayln "Exercise 6, totalStat:")
(totalStat points Team_MADEC)    ; => 86
(totalStat assists Team_MADEC)    ; => 29
(totalStat rebounds Team_MADEC)    ; => 33
