(require mzlib/compat racket/function)

; Name and surnames:

;___________________________________________________________________________
;
;          NOTES IN RELATION TO THE ANSWERS OF THE EXERCISES
;
; 1. It is COMPULSORY to do the analsys of cases for every recursive function.
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

;;; 1. Base       : the result of substract(l1, l2) when l2 is the empty list;
;;;                 substract(l1, l2) = l1
;;; 2. Recurrence: l2 is not the empty list; that is, l2=cons(car(l2), cdr(l2)) and l1 and l2 are lists
;;;     Hypothesis: assume we know substract(l1 (cdr l2)) = H
;;;         Thesis: substract (l1 ,l2)= l1- H
;;;


(define (substract l1 l2)
                  (if (and (list? l1) (list? l2))
                      (if (null? l2 )
                          l1
                          (filter (lambda (x)(not (equal? (car l2) x)))
                                  (substract l1 (cdr l2))))
                      l1))

 

(displayln "Exercise 1, substract:")
;(substract '(1 2 3 4 5 6) '(3 5 6))    ; => (1 2 4)
;(substract '(4 8 5 6 4 12) '(3 11 8 9 4))    ; => (5 6 12)


;; Exercise 2 (1,5 points) - Function contains?(S-expr, x)
;;                Given an S-expression S and an atom x, returns true if x is contained in S and
;;                false if not.
;;                The function must be recursive, and therefore the case analysis must be done.
;;

;;; 1. Base       : S-expr is not a list; (equal? S-expr x)               
;;; 2. Recurrence: if S-expr is a list;
;;;         Hypothesis: assume we know contains ((cdr S-expr) x)= H    
;;;         Thesis: contains (S-expr ,x)= true if (car S-expr) is x or H
;;;

(define (contains S-expr x)
  (if (null? S-expr)
    #f
    (if (or (equal? (car S-expr) x) (and (list? (car S-expr)) (contains (car S-expr) x)))
      #t
      (contains (cdr S-expr) x)
    )
  )
)


(displayln "Exercise 2, contains?:")
;(contains? '(a (b ((c (d) (a))))) 'a)    ; => #t
;(contains? '(a (b ((c (d) (a))))) 'd)    ; => #t
;(contains? '(a (b ((c (d) (a))))) 'f)    ; => #f


;; 'Team_MADEC' stores information about the players of a basketball team.
;; Every register has the form (name (points assists rebounds))

(define Team_MADEC '((Marta (15 3 5))
               (Alvaro (21 7 2))
               (Daniel (11 4 8))
               (Eva (30 2 11))
               (Carlota (9 13 7))))

;; --------------------------------------------------
;; Solve with HOF the following exercises
;; --------------------------------------------------

;; Exercise 3 (1,5 points) - Functions points(name, team), assists(name, team) and rebounds(name, team)
;;              Given a name and a team like the one presented, define three functions points,
;;              assists and rebounds that return the asked information about the given player
;;

(define (get-stats name team)
  (cadr (findf (lambda (x) (equal? (car x) name)) team))
)
               
(define (points name team)
  (list-ref (get-stats name team) 0)
) 

(define (assists name team)
  (list-ref (get-stats name team) 1)
) 

(define (rebounds name team)
  (list-ref (get-stats name team) 2)
) 


(displayln "Exercise 3, points, assists, rebounds:")
;(points 'Alvaro Team_MADEC)    ; => 21
;(assists 'Alvaro Team_MADEC)    ; => 7
;(rebounds 'Alvaro Team_MADEC)    ; => 2


;; Exercise 4 (1.5 points) -  Function maxStat(name, team)
;;              Given a team and a player's name, returns the score he/she achieved for
;;              his/her best stat (points, assists or rebounds)
;;

(define (maxStat name team)
  (apply max (get-stats name team))
)


(displayln "Exercise 4, maxStat:")
;(maxStat 'Marta Team_MADEC)    ; => 15
;(maxStat 'Daniel Team_MADEC)    ; => 11
;(maxStat 'Carlota Team_MADEC)    ; => 13


;; Exercise 5 (1.5 points) - Function double-double?(team)
;;              Given a team, returns true if there is a player that achieved a double double.
;;              In basketball, a double double is achieved when a player gets 10 or more in
;;              at least two stats. For example, 12 points and 10 assists.
;;

(define (double-double team)
  (not (null? (filter
    (lambda (x) (> x 1))
    (map 
      (lambda (plist) (length (filter (lambda (x) (>= x 10)) plist))) 
      (map (lambda (t) (cadr t)) team)
    )
  )))
)

(displayln "Exercise 5, double-double?:")
;(double-double? Team_MADEC)    ; => #t


;; Exercise 6 (1.5 points) - Function totalStat(stat, team)
;;              Given a team and a stat ('points', 'assists' or 'rebounds'), returns the
;;              total score of all the team's players for the given stat.
;;

(define (totalStat stat-name team)
  (apply + (map 
    (lambda (name) (stat-name name team)) 
    (map (lambda (t) (car t)) team)
  ))
)



(displayln "Exercise 6, totalStat:")
;(totalStat points Team_MADEC)    ; => 86
;(totalStat assists Team_MADEC)    ; => 29
;(totalStat rebounds Team_MADEC)    ; => 33


