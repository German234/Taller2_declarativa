#lang racket

;Ejercicio 1 – Contar elementos positivos en una lista

(define lista '(3 -2 7 0 -5 9))

(define positivos
  (filter (lambda (x) (> x 0)) lista))

(displayln "Ejercicio 1:")
(displayln (string-append
  (number->string (length positivos))
  " elementos positivos"))
(newline)


;Ejercicio 2 – Generar lista de cuadrados pares

(define lista2 '(1 2 3 4 5 6 7 8))

(define cuadrados-pares
  (map (lambda (x) (* x x))
       (filter (lambda (x) (even? x)) lista2)))

(displayln "Ejercicio 2:")
(displayln cuadrados-pares)
(newline)


;Ejercicio 3 – Calcular el factorial de un número

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3:")
(displayln (factorial 5)) 
(newline)


;Ejercicio 4 – Elevar cada número al cubo

(define lista4 '(2 3 4))

(define cubos (map (lambda (x) (* x x x)) lista4))

(displayln "Ejercicio 4:")
(displayln cubos)
(newline)


;Ejercicio 5 – Sumar todos los elementos impares

(define lista5 '(1 2 3 4 5 6 7))

(define suma-impares
  (foldl + 0 (filter (lambda (x) (odd? x)) lista5)))

(displayln "Ejercicio 5:")
(displayln suma-impares)
(newline)


;Ejercicio 6 – Determinar si una lista contiene números negativos

(define lista6 '(5 9 -3 2))

(define contiene-negativos
  (ormap (lambda (x) (< x 0)) lista6))

(displayln "Ejercicio 6:")
(displayln contiene-negativos)
(newline)


;Ejercicio 7 – Calcular la suma acumulada de una lista

(define (suma-acumulada lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))

(displayln "Ejercicio 7:")
(displayln (suma-acumulada '(1 2 3 4)))
(newline)


;Ejercicio 8 – Concatenar cadenas de texto en una lista

(define lista8 '("Hola" " " "Mundo"))

(define concatenado
  (foldl string-append "" (reverse lista8)))

(displayln "Ejercicio 8:")
(displayln concatenado) 
(newline)


;Ejercicio 9 – Generar lista con el doble de los números mayores que 5

(define lista9 '(3 6 8 2 10))

(define dobles-mayores5
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista9)))

(displayln "Ejercicio 9:")
(displayln dobles-mayores5)
(newline)


;Ejercicio 10 – Invertir el orden de una lista

(define (invertir lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

(displayln "Ejercicio 10:")
(displayln (invertir '(1 2 3 4))) 
(newline)


;Ejercicio 11 – Crear una función que reciba una función como parámetro

(define (aplicar-funcion f lista)
  (map f lista))

(displayln "Ejercicio 11:")
(displayln (aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4))) 
(newline)


;Ejercicio 12 – Reto integrador: combinar múltiples funciones

(define lista12 '(3 8 10 4 9 2 7))

(define mayores5 (filter (lambda (x) (> x 5)) lista12))

(define procesados (map (lambda (x) x) mayores5)) 

(define suma (foldl (lambda (x acc) (+ x acc)) 0 procesados))

(define promedio (/ suma (length procesados)))

(displayln "Ejercicio 12:")
(displayln promedio)
(newline)