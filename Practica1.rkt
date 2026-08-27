#lang Racket

;; Integrantes del equipo
;; Irving Axel Tapia Anrubio - 425019526
;; Brayan Alexis Gallardo Valdez - 322099449
;; Gustavo Adrian Murillo Espinosa - 317011375

;; Ejercicio 1
;; password aceptable? : String -> Boolean
;; Se considera aceptable si tiene por lo menos 8 caracteres, tiene al menos un numero, tiene al menos una letra mayuscula

;; Auxiliar para mas facil
(define (contiene-digito? s)
(contiene-digito-auxiliar? s 0))

;; los dos caminos posibles para digitos
(define (contiene-digito-auxiliar? s i)
(cond
   [(= i (string-length s)) #f]
   [(char-numeric? (string-ref s i)) #t]
   [else (contiene-digito-auxiliar? s (+ i 1))]))


;; Checar si tiene mayuscula
(define (contiene-mayuscula? s)
   (contiene-mayuscula-auxiliar? s 0))


;; Igual que el pasado, usamos como switch
(define (contiene-mayuscula-auxiliar? s i)
(cond 
     [(= i (string-length s)) #f]
     [(char-upper-case? (string-ref s i)) #t]
     [else (contiene-mayuscula-auxiliar? s (+ i 1))]))


;; Ya con los dos de arriba solo es checar que ambos sean verdaderos con un and
(define (password-aceptable? password)
   (and (>= (string-length password) 8)
        (contiene-digito? password)
        (contiene-mayuscula? password)))



;; Para probar
 (password-aceptable? "Racket2027")
;;debe dar #t
 (password-aceptable? "racket2027")
;;debe dar #f
 (password-aceptable? "Ra2")
;;debe dar #f


;; Ejercicio 2
;; en-rango? : Number Number Number -> Boolean
;; triangulo-valido? : Number Number Number -> Boolean

(define (triangulo-valido? a b c)
(and (> a 0) (> b 0) (> c 0)
(> (+ a b) c)
(> (+ a c) b)
(> (+ b c) a)))

;;Para probar
(triangulo-valido? 3 4 5)
;;debe dar #t
 (triangulo-valido? 5 5 5)
;;debe dar #t
 (triangulo-valido? -1 4 5)
;;debe dar #f


;; Ejercicio 3
;; letras-repetidas? : String -> Boolean
;; hay que checar si la letra que tenemos en la posicion 0 se encuentra despues en 
;; el string y si no pues seguimos con la siguiente letra y asi hasta terminar el string

(define (aparece-luego? s c i)
  (cond 
       [(= i (string-length s)) #f]
       [(char=? (string-ref s i) c) #t]
       [else (aparece-luego? s c (+ i 1))]))

;; el principal, pero falta hacerlo por cada letra
(define (letras-repetidas? s)
 (letras-repetidas-auxiliar? s 0))

;; aqui ya por cada letra
(define (letras-repetidas-auxiliar? s i)
 (cond 
      [(= i (string-length s)) #f]
      [(aparece-luego? s (string-ref s i) (+ i 1)) #t]
      [else (letras-repetidas-auxiliar? s (+ i 1))]))



;; Para probar
 (letras-repetidas?  "hola")
;;debe dar #false
 (letras-repetidas? "casa")
;;debe dar #true


;; Ejercicio 4
;; anagrama-profundo? : String String -> Boolean

;;(define(anagrama-profundo? s1 s2))


;; Para probar
;; (anagrama-profundo? "abcde" "edcba") debe dar #true
;; (anagrama-profundo; "abcde" "xyzde") debe dar #false



;; Ejercicio 5
;; escalera? : (List of Number) -> Boolean

(define (escalera? lst)
  (cond
    [(empty? lst) #t]
    [(empty? rest lst) #t]
    ;;[]
    [else #f]))

;; Para probar
;; (escalera '())
;; debe dar #t
;; (escalera '(3 4 5 7))
;; debe dar #f
;; (escalera '(-2 -1 0 1))
;; debe dar #t


;; Ejercicio 6
(list [+ 1 2]
      [* 2 3]
      [- 10 3])

(cons 'hola '(mundo))

(cons 5 [cons 4 {cons 3 '(4)}])


;; Ejercicio 7
;; intercalar : (listof Any) (listof Any) -> (listof Any)
(define (intercalar a b)
  (cond
    [(empty? a) b]
    [(empty? b) a]
    [else (cons (first a)
                (cons (first b)
                      (intercalar (rest a) (rest b))))]))

;; Para probar
(intercalar '(1 2 3) '(a b c))
(intercalar '() '(c d))
(intercalar '(1 2) '())



;; Esto lo hice en laboratorio de lenguajes
(define numeros '(10 20 30 40))

(car (cdr (cdr numeros)))



(define (suma-uno lista)
(if (empty? lista)
'()
(cons (+ 1 (first lista))
(suma-uno (rest lista)))))

(suma-uno '(1 2 3))



;; Ejercicios ayudantia















