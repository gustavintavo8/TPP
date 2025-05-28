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

; Solution iterating through l1 and checking if each element is in l2.
; If the element is in l2, then it is removed.
; Else, it is left.

; 1. Base: l1 is the empty list. Then substract((), l2) = ()
; 2. Recursive: l1 is not the empty list. Then l1 is cons(car(l1), cdr(l1))
;   2.1: Assume we know substract(cdr(l1), l2) = H
;   2.2: substract(l1, l2) = If car(l1) is in l2, then H
;                            else cons(car(l1), H)

(define substract
  (lambda rest
    (letrec ((f (lambda (l1 l2)
                  (cond [(null? l1) ()]
                        [(member (car l1) l2) (substract (cdr l1) l2)]
                        [else (cons (car l1) (substract (cdr l1) l2))]))))
      (cond [(not (= (length rest) 2)) (error "substract(A, B): Must receive two arguments.")]
            [(not (list? (car rest))) (error "substract(A, B): A must be a list.")]
            [(not (list? (cadr rest))) (error "substract(A, B): B must be a list.")]
            [else (f (car rest) (cadr rest))]))))


(displayln "Exercise 1, substract:")
;(substract '(1 2 3 4 5 6) 3) ; => error (B is not a list)
;(substract 3 '(3 5 6)) ; => error (A is not a list)
;(substract '(1 2 3 4 5 6) '(3 5 6) 3) ; => error (Not two arguments)
(substract '(1 2 3 4 5 6) '(3 5 6))    ; => (1 2 4)
(substract '(4 8 5 6 4 12) '(3 11 8 9 4))    ; => (5 6 12)


;; Exercise 2 (1,5 points) - Function contains?(S-expr, x)
;;                Given an S-expression S and an atom x, returns true if x is contained in S and
;;                false if not.
;;                The function must be recursive, and therefore the case analysis must be done.
;;

; 1. Base: Sexp is the empty list. Then contains?((), x) = #f
;          Sexp is an atom. Then if Sexp=x => #t
;                                else #f
; 2. Recursive: Sexp is not the empty list. Then Sexp is cons(car(Sexp), cdr(Sexp))
;   2.1. Hypothesis: Assume we know contains?(car(Sexp), x) = H1 and contains?(cdr(Sexp), x) = H2
;   2.2. Thesis: Then contains?(Sexp, x) = or(H1, H2).

(define (contains? Sexp x)
  (cond [(null? Sexp) #f]
        [(atom? Sexp) (if (equal? Sexp x) #t #f)]
        [else (or (contains? (car Sexp) x) (contains? (cdr Sexp) x))]))

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
  (caadar (filter (curry member name) team)))

(define (assists name team)
  (car (cdadar (filter (curry member name) team))))

(define (rebounds name team)
  (cadr (cdadar (filter (curry member name) team))))

(displayln "Exercise 3, points, assists, rebounds:")
(points 'Alvaro Team_MADEC)    ; => 21
(assists 'Alvaro Team_MADEC)    ; => 7
(rebounds 'Alvaro Team_MADEC)    ; => 2


;; Exercise 4 (1.5 points) -  Function maxStat(name, team)
;;              Given a team and a player's name, returns the score he/she achieved for
;;              his/her best stat (points, assists or rebounds)
;;

; 1. First, get all the stats of the given person with filter.
; 2. Then, select the maximum one.

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


; 1. First, the stats of each player are extracted.
; 2. With (p, a, r) of each player, only those greater than 10 are left.
; 3. Then we keep only the the players with 2 or more remaining stats.
; 4. If the remaining list is not null, that means that there are players that meet those conditions.
; 5. Therefore, it is a double-double team.

;(define (double-double? team)
;  (not (null? (filter (compose (curry <= 2) length) (map (compose (curry filter (curry <= 10)) cadr) team)))))

(define (double-double? team)
  (not (null? (filter (compose (curry <= 2) length) (map (compose (curry filter (curry <= 10)) cadr) team)))))

(displayln "Exercise 5, double-double?:")
(double-double? Team_MADEC)    ; => #t


;; Exercise 6 (1.5 points) - Function totalStat(stat, team)
;;              Given a team and a stat ('points', 'assists' or 'rebounds'), returns the
;;              total score of all the team's players for the given stat.
;;

; Here we cannot use curry, as "x" is in the middle of the lambda. This function works but I'll do a version with curry below.
(define (totalStat stat team)
  (apply + (map (lambda (x) (stat x team)) (map car team))))

; If we want to use curry, we should redefine the stats functions so x is the last parameter, as follows:
(define (points team name) (caadar (filter (curry member name) team)))
(define (assists team name) (car (cdadar (filter (curry member name) team))))
(define (rebounds team name) (cadr (cdadar (filter (curry member name) team))))

; Now we can currify our function.
(define (totalStat stat team)
  (apply + (map (curry stat team) (map car team))))

(displayln "Exercise 6, totalStat:")
(totalStat points Team_MADEC)    ; => 86
(totalStat assists Team_MADEC)    ; => 29
(totalStat rebounds Team_MADEC)    ; => 33


