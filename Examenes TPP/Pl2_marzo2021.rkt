(require mzlib/compat racket/function)

(define asociaciones '((sexp (a (b))) (numero1 2.5)
                       (lista1 (a c)) (numero1 0.25)
                       (string "hola") (numero2 10) 
                       (numero1 -3.2) (lista1 (b a))
                       (lista1 (e c f)) (lista2 (a d f))
                       (sexp ((c d) e)) (numero2 24.7)
                       (numero1 -12) (numero2 -0.8)
                       (lista3 (e)) (symbol uno)
                       (mix 2) (numero3 -4)
                       (mix a) (mix (e f))))

;; Extrae(Datos, Filtro, Formato) => (...)
;;
;; Función de orden superior que recibe:
;; Datos                  : los datos a examinar (las asociaciones)
;; Filtro(asoc)  => #t/#f : función de filtrado que se aplica a cada asociación
;; Formato(asoc) => (...) : función que retorna la información relevante,
;;                          o de interés, de una asociación

(define (Extrae Datos Filtro Formato)
  (map Formato (filter Filtro Datos)))

;--------------------------------------------------------------------------
; EJERCICIO 1 (2 ptos)
; -----------
; Utiliza la función dada 'Extrae' para definir la función valores(key, Datos).
; Dada una clave simbólica y la lista 'asociaciones', la función solicitada
; retorna la lista de todos los valores asociados a la clave dada.
;--------------------------------------------------------------------------




;(valores 'lista1 asociaciones)  ;=> ((a c) (b a) (e c f))

;--------------------------------------------------------------------------
; EJERCICIO 2 (2 a 2.5 ptos)
; -----------
; Define la función recursiva claves(lista-asoc) que dada una lista de
; asociaciones (lista de pares clave/valor) retorne la lista de las
; distintas claves existentes. Es indispensable proporcionar el análisis
; por casos y, además, se valorará evitar cálculos repetitivos (0.5 ptos).
;
; Finalmente define el símbolo las-claves como la lista de las distintas
; claves que hay en la lista dada 'asociaciones'
;--------------------------------------------------------------------------





; las-claves  ;=> (string lista1 lista2 sexp numero1
              ;    numero2 lista3 symbol numero3 mix)

;--------------------------------------------------------------------------
; EJERCICIO 3 (2.75 ptos)
; -----------
; Utiliza el símbolo definido previamente, 'las-claves', y la función
; 'valores' definida en el ejercicio 1, para proporcionar una expresión
; FOS que retorne las 'asociaciones' en la forma:
;
; ((string ("hola"))
; (lista1 ((a c) (b a) (e c f)))
; (lista2 ((a d f)))
; (sexp ((a (b)) ((c d) e)))
; (numero1 (2.5 0.25 -3.2 -12))
; (numero2 (10 24.7 -0.8))
; (lista3 ((e)))
; (symbol (uno))
; (numero3 (-4))
; (mix (2 a (e f))))
;
; que asocia cada clave distinta con su lista de valores.
;--------------------------------------------------------------------------



  
;--------------------------------------------------------------------------
; EJERCICIO 4 (2.75 ptos) lo cambiaron por el otro ejercicio 4
; -----------
; Dar un definición basada en FOS de la función claves(lista-asoc) del
; ejercicio 2.
;--------------------------------------------------------------------------

;--------------------------------------------------------------------------
; EJERCICIO 4 (2.75 ptos) 
; -----------
;dar una función basada en FOS de la función sumas-total(lista)
;que retorna la suma de todos los valores numéricos de la lista asociaciones
; ejercicio 2.
;--------------------------------------------------------------------------

