#lang Racket

;; Integrantes del equipo
;; Irving Axel Tapia Anrubio - 425019526
;; Brayan Alexis Gallardo Valdez - 322099449
;; Gustavo Adrian Murillo Espinosa - 317011375

;; Ejercicio 1
;; Se considera aceptable si tiene por lo menos 8 caracteres, tiene al menos un numero, tiene al menos una letra mayuscula

;; Auxiliar para mas facil

;;String -> Boolean
(define (contiene-digito? s)
(contiene-digito-auxiliar? s 0))

;; los dos caminos posibles para digitos
;; String Number -> Boolean
(define (contiene-digito-auxiliar? s i)
(cond
   [(= i (string-length s)) #f]
   [(char-numeric? (string-ref s i)) #t]
   [else (contiene-digito-auxiliar? s (+ i 1))]))


;; Checar si tiene mayuscula
;; String -> Boolean
(define (contiene-mayuscula? s)
   (contiene-mayuscula-auxiliar? s 0))


;; Igual que el pasado, usamos como switch
;; String Number -> Boolean
(define (contiene-mayuscula-auxiliar? s i)
(cond 
     [(= i (string-length s)) #f]
     [(char-upper-case? (string-ref s i)) #t]
     [else (contiene-mayuscula-auxiliar? s (+ i 1))]))


;; Ya con los dos de arriba solo es checar que ambos sean verdaderos con un and
;;String -> Boolean
(define (password-aceptable? password)
   (and (>= (string-length password) 8)
        (contiene-digito? password)
        (contiene-mayuscula? password)))



;; Para probar
;; (password-aceptable? "Racket2027")
;;debe dar #t
;; (password-aceptable? "racket2027")
;;debe dar #f
;; (password-aceptable? "Ra2")
;;debe dar #f


;; Ejercicio 2

;; Usamos and y > para validar las reglas de que se puede formar un triangulo (que todos sus lados sean positivos
;; y que la suma de cualesquiera dos lados debe ser mayor que el tercero)

;; triangulo-valido? : Number Number Number -> Boolean
(define (triangulo-valido? a b c)
(and (> a 0) (> b 0) (> c 0)
(> (+ a b) c)
(> (+ a c) b)
(> (+ b c) a)))

;;Para probar
;;(triangulo-valido? 3 4 5)
;;debe dar #t
;;(triangulo-valido? 5 5 5)
;;debe dar #t
;;(triangulo-valido? -1 4 5)
;;debe dar #f


;; Ejercicio 3

;; hay que checar si la letra que tenemos en la posicion 0 se encuentra despues en 
;; el string y si no pues seguimos con la siguiente letra y asi hasta terminar el string

;; String Char Number -> Boolean
(define (aparece-luego? s c i)
  (cond 
       [(= i (string-length s)) #f]
       [(char=? (string-ref s i) c) #t]
       [else (aparece-luego? s c (+ i 1))]))

;; el principal, pero falta hacerlo por cada letra
;; String -> Boolean
(define (letras-repetidas? s)
 (letras-repetidas-auxiliar? s 0))

;; aqui ya por cada letra
;; String Number -> Boolean
(define (letras-repetidas-auxiliar? s i)
 (cond 
      [(= i (string-length s)) #f]
      [(aparece-luego? s (string-ref s i) (+ i 1)) #t]
      [else (letras-repetidas-auxiliar? s (+ i 1))]))



;; Para probar
;;(letras-repetidas?  "hola")
;;debe dar #false
;;(letras-repetidas? "casa")
;;debe dar #true


;; Ejercicio 4
;; Comprueba recursivamente si los caracteres de una lista existen en la otra y los va removiendo.

;; Vuelve los string a listar para aislar y procesar cada letra de manera individual
;; String String -> Boolean
(define (anagrama-profundo? s1 s2)
  (anagrama-listas? (string->list s1) (string->list s2)))

;; Tras checar que sean igual de largas ambas listas, revisa si el 1er
;; caracter de la 1ra lista existe en la 2da. En el caso de que sea asi,
;; hace la llamada recursiva procesando el rest de la 1ra y mandando a eliminar
;; ese caracter en la otra lista
;; (Listof Char) (Listof Char) -> Boolean
(define (anagrama-listas? l1 l2)
  (cond
    [(not (= (length l1) (length l2))) #f]
    [(empty? l1) (empty? l2)]
    [(member (first l1) l2)
     (anagrama-listas? (rest l1) (remover-primero (first l1) l2))]
    [else #f]))

;; Reconstruye la 2da lista con cons, saltandose unicamente la 1ra coincidencia del
;; 1er caracter buscado para asegurar que si hay letras duplicadas, se emparejen correctamente
;; Char (Listof Char) -> (Listof Char)
(define (remover-primero x lst)
  (cond
    [(empty? lst) empty]
    [(char=? x (first lst)) (rest lst)]
    [else (cons (first lst) (remover-primero x (rest lst)))]))


;; Para probar
;;(anagrama-profundo? "abcde" "edcba")
;; debe dar #true
;;(anagrama-profundo? "abcde" "xyzde")
;;debe dar #false



;; Ejercicio 5



;; Si esta vacia o tiene un elemento, da "#t" luego luego, en caso de que haya mas,
;; verifica que el 2do elemento de la lista sea exactamente el 1ro mas uno, avanza
;; de manera recursiva
;; (List of Number) -> Boolean
(define (escalera? lst)
  (cond
    [(empty? lst) #t]
    [(empty? (rest lst)) #t]
    [(= (first (rest lst)) (+ 1 (first lst))) (escalera? (rest lst))]
    [else #f]))

;; Para ver que amarre
;;(escalera? '())
;; debe dar #t
;;(escalera? '(3 4 5 7))
;; debe dar #f
;;(escalera? '(-2 -1 0 1))
;; debe dar #t


;; Ejercicio 6

;; No es mucho por explicar, pero por si acaso: Se obtiene directamente con
;; string-ref tras calcular la longitud de la cadena menos 1 (puesto que empiezan por 0)
;; String -> Char
(define (ultimo-caracter cadena)
  (string-ref cadena (- (string-length cadena) 1)))
   
;;Para ver que amarra
;;(ultimo-caracter "Racket")
;;debe dar t
;;(ultimo-caracter "hola")
;; debe dar a
;;(ultimo-caracter "7")
;; debe dar 7



;; Ejercicio 7

;; Se anidan los cons para enlazar el first de la lista a seguido por el first de la lista b,
;; se sigue la recursion con el rest de ambas hasta que alguna termine
;; (listof Any) (listof Any) -> (listof Any)
(define (intercalar a b)
  (cond
    [(empty? a) b]
    [(empty? b) a]
    [else (cons (first a)
                (cons (first b)
                      (intercalar (rest a) (rest b))))]))

;; Para probar
;;(intercalar '(1 2 3) '(a b c))
;; debe regresar (1 a 2 b 3 c)
;;(intercalar '() '(c d))
;; debe regresar (c d)
;;(intercalar '(1 2) '())
;; debe regresar (1 2)


;; Ejercicio 8

;; Al hacer la llamada recursiva resta el calculo del resto de la lista
;; al 1er elemento, y al resolverse de atras hacia adelante, la resta sobre
;; otra resta invierte los signos (si era resta, ahora es suma y viceversa),
;; lo que da el efecto donde se alterna la suma y la resta
;;(Listof Number) -> Number
(define (zigzag-sum lst)
  (if (empty? lst)
      0
      (- (first lst) (zigzag-sum (rest lst)))))

;;Para ver que si amarra
;;(zigzag-sum '())
;; 0 debera devolver al correr
;;(zigzag-sum '(1 2 3 4 5))
;; debera devolver 3, tras hacer  1 - 2 + 3 - 4 + 5