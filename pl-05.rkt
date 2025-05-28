(require mzlib/compat racket/function)

;; Ejercicio 1
;; ===========

;; La función union(A, B) retorna el conjunto unión de los dos dados.
(define (union A B)
  (append
   (filter (lambda(x) (not (member x B))) A)
   B))

;; a.


;(display "\nunion-all(((a (b) d) (k (c)) (f d e a))): ")
;(union-all '((a (b) d) (k (c)) (f d e a)))

;; b.



;; c.


;(display "\nunion-all(): ")
;(union-all)
;(display "\nunion-all(((a (b) d) c (f d e a))): ")
;(union-all '(((a (b) d) c (f d e a)))
;(display "\nunion-all(((a (b) d) (k (c)) (f d e a))): ")
;(union-all '((a (b) d) (k (c)) (f d e a)))


;; Ejercicio 2
;; ===========

;; Tabla de precipitaciones acumuladas mensuales medias entre los años 1981 y 2010 en
;; las comunidades autónomas (se incluyen las ciudades autónomas de Ceuta y Melilla).
;; Estas se utilizan como referencia de valores normales de precipitación.
(define Precipitaciones_1981-2010
  '((Mes Asturias Baleares Cantabria Ceuta La_Rioja Madrid Melilla Murcia Navarra Canarias Andalucía 
        Aragón Castilla-La_Mancha Castilla_y_León Cataluña Extremadura Galicia País_Vasco Comunitat_Valenciana)
    (Ene 118.2 53.2 125.7  85.5 43.5  45.3  55.6  25.4  85.8  41.3  68.8 33.9  41.5  58.8 43.5  68.4 155.3 121.4  42.1)
    (Feb 107.3 46.4 105.0  96.6 36.0  40.2  55.6  28.9  73.8  43.7  58.5 29.2  38.4  46.0 32.9  55.2 121.8  98.7  38.3)
    (Mar 102.2 38.0 107.0  66.3 37.8  32.0  42.2  29.6  74.4  33.5  46.1 30.5  33.7  41.4 38.5  41.7 108.1 101.0  35.2)
    (Abr 122.2 45.3 115.0  61.1 62.3  53.0  29.4  29.3  98.7  15.8  56.3 56.9  55.0  61.4 60.0  59.3 120.1 114.9  49.6)
    (May  99.7 39.6  97.8  26.7 68.9  57.2  19.2  33.1  79.4   5.6  39.7 64.0  52.0  65.3 70.3  50.2 102.0  93.1  49.8)
    (Jun  65.1 13.9  62.6   6.0 48.7  28.1   6.8  18.7  54.5   2.1  14.4 44.0  30.4  36.7 50.3  21.0  55.0  62.4  27.3)
    (Jul  47.3  6.8  49.2   1.3 31.7  11.6   0.8   4.2  38.9   0.8   2.9 26.9   9.4  20.2 31.4   5.3  36.1  50.1  11.8)
    (Ago  56.2 21.8  61.7   5.8 27.5  12.8   3.6  11.0  42.7   1.7   6.9 34.4  13.0  21.4 51.6   6.9  41.0  62.0  21.9)
    (Sep  74.0 66.1  74.7  24.0 33.5  29.0  17.1  34.8  61.3   7.2  29.8 48.8  32.5  40.0 67.8  33.4  79.7  72.1  60.4)
    (Oct 127.8 80.5 111.3  68.9 52.2  67.3  38.9  38.5  89.0  27.1  67.4 58.8  59.1  74.9 77.4  80.8 170.7 109.7  70.0)
    (Nov 150.8 83.4 153.0 106.1 55.4  68.5  54.5  36.7 103.8  47.0  85.0 49.2  55.1  73.1 60.5  87.4 175.6 142.9  56.9)
    (Dic 139.4 70.0 130.4 148.9 53.3  66.2  48.2  26.4  99.7  64.5 100.1 42.9  61.2  77.9 49.8  94.3 189.9 128.2  47.5)))

;; Tabla de precipitaciones acumuladas mensuales del año 2020 en las comunidades autónomas
;; (se incluyen las ciudades autónomas de Ceuta y Melilla)
(define Precipitaciones_2020  
  '((Mes Asturias Baleares Cantabria Ceuta La_Rioja Madrid Melilla Murcia Navarra Canarias Andalucía 
        Aragón Castilla-La_Mancha Castilla_y_León Cataluña Extremadura Galicia País_Vasco Comunitat_Valenciana)
    (Ene  55.2 14.8 105.9  30.4 16.6  12.7   3.0   9.9  91.5  37.0   7.3 11.1   8.4  14.7 12.8   5.4  53.0 129.2   8.1)
    (Feb  42.5  2.0  35.6  16.0  6.1  10.3   5.6   3.1  19.2  30.8  10.9  5.6  10.4   7.5  8.0   5.8  40.6  35.9   8.5)
    (Mar 121.7 66.2 132.7 219.3 64.8 120.4  69.3 212.5  83.7  41.9 150.1 70.3  97.2  80.6 93.5 102.3 113.3 108.7 272.8)
    (Abr 121.3 41.0 127.6  81.8 72.0  64.1 113.0  88.0  89.3  21.2  68.4 76.4  73.0  59.9 59.1  57.3  78.3 122.2  97.2)
    (May  41.7 35.7  19.0   2.2 22.5  10.2   4.8  36.8  12.1   2.8  16.1 26.8  12.0  16.7 39.1   3.8  41.1  25.9  55.6)
    (Jun  95.4  1.7  56.5   0.1 10.4   3.1   0.2   4.5  21.9   3.8   1.0 22.6   3.7  21.3 22.8   4.8  79.7  48.5   9.5)
    (Jul  18.1  0.4  13.5   0.0 13.2   6.3   3.2   0.6  20.5   1.6   0.1 32.6   4.5   5.3 25.0   0.1   8.9  14.0   9.1)
    (Ago  71.3 27.1  64.2   0.2 31.7   5.4   0.6  11.1  34.0   4.6   4.2 38.3  14.5  21.4 78.4   1.2  26.5  39.2  22.5)
    (Sep  69.2 73.1  90.6   9.0 20.4  44.6   2.1  28.1  44.0 122.5  16.7 32.1  26.6  35.9 59.2  40.6  77.8  74.7  42.6)
    (Oct  62.3 60.3  38.4   9.2 28.1  46.6   5.0  25.4  28.9   8.6  18.8 32.8  30.7  81.3 32.5  71.8 216.3  21.3  45.4)
    (Nov 120.0 95.7 129.0  67.3 56.8  60.3   9.4  15.1 126.8   4.7  30.7 56.2  46.0  79.1 28.9  66.7 228.3 162.4  65.3)
    (Dic 106.1 40.2  82.0 177.6 59.3 129.9  43.2  18.2  63.1  53.4 152.7 55.5 125.8 127.8 38.5 231.3 293.1  58.6  35.2)))

;; a.


;(displayln "\nprec-mes(cadr(Precipitaciones_2020)):")
;(prec-mes (cadr Precipitaciones_2020))

;; b.


;(displayln "\nprec-mes-españa(Precipitaciones_2020):")
;(prec-mes-españa Precipitaciones_2020)

;; c.


;(displayln "\nprec-t-mes(Precipitaciones_2020, cadr):")
;(prec-t-mes Precipitaciones_2020 cadr)

; d.
(displayln "\nPrecipitaciones mensuales en Cantabria en 2020:")


(displayln "\nPrecipitaciones mensuales en Ceuta en 2020:")


(displayln "\nPrecipitaciones mensuales en España en 2020:")




;; Ejercicio 3
;; ===========

;; Porcentajes de la diferencias entre las precipitaciones mensuales en España en el
;; año 2020 y la referencia de valor normal de precipitación mensual en España (la
;; media de las precipitación mensuales entre los años 1981-2010).
(define Porcentajes_2020
  '((Ene -52.25) (Feb -73.59) (Mar 113.75) (Abr 19.4) (May -61.84) (Jun -36.5)
    (Jul -54.23) (Ago -1.49) (Sep 2.66) (Oct -41.26) (Nov -11.93) (Dic 15.42)))

;; a.
(display "\nNúmero de meses que tuvieron una precipitación próxima al valor normal en 2020: ")


;; b.
(display "\nLista de los meses con una precipitación inferior al valor normal en 2020: ")




;; Ejercicio 4
;; ===========

;; Precipitaciones mensuales medias en España entre los años 1981 y 2010.
;; Estas se utilizan como referencia de valor normal de precipitación mensual.
(define Prec_Medias_1981-2010
  '((Ene 1313.2) (Feb 1152.5) (Mar 1039.2) (Abr 1265.6) (May 1113.6) (Jun 648.0)
    (Jul 386.7) (Ago 503.9) (Sep 886.2) (Oct 1470.3) (Nov 1644.9) (Dic 1638.8)))

;; Precipitaciones mensuales en España en el año 2020
(define Prec_España_2020
  '((Ene 627.0) (Feb 304.4) (Mar 2221.3) (Abr 1511.1) (May 424.9) (Jun 411.5)
    (Jul 177.0) (Ago 496.4) (Sep 909.8) (Oct 863.7) (Nov 1448.7) (Dic 1891.5)))



