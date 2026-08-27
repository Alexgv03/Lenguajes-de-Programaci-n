#lang Racket

;; Maraton Racket    CRACKETS

;; cuadrado
(define (cuadrado x)
  (* x x))

(cuadrado 5)

;; multiplo
(define (multiplo? a b)
  (cond
    [(= 0 (modulo a b)) #t]
    [else #f]))

(multiplo? 4 2)


;; esta en la lista
(define (esta? a b)
  (cond
    [(empty? b) #f]
    [= a (car b) #t]
    [else (esta? a (cdr b))]))

(esta? 5 '())
(esta? 5 '(4 3 5))


;; Duplicar elementos es el 36
(define (duplicar lst)
  (cond
    [(empty? lst) '()]
    [else (cons (first lst)
                (cons (first lst)
                      (duplicar (rest lst))))]))

(duplicar '(1 2 3))


;; Agregar inicio 12
(define (agregar-inicio a b)
  (cons a b))

(agregar-inicio 1 '(2 3 4))

;; Eliminar 35
(define (eliminar x y)
  (cond
    [(empty? y) '()]
    [(equal? (first y) x) (eliminar x (rest y))]
    [else (cons (first y) (eliminar x (rest y)))]))

(eliminar 1 '(1 2 3))


;; Contar hasta n   es el 26
(define (contar-hasta n)
  (define (contador actual)
    (cond
      [(> actual n) '()]
      [else (cons actual (contador (+ actual 1)))]))
  (contador 0))

(contar-hasta 10)

