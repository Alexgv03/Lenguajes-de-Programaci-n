#lang plai
(require (file "./grammars.rkt"))

;; Integrantes del equipo
;; Irving Axel Tapia Anrubio - 425019526
;; Brayan Alexis Gallardo Valdez - 322099449
;; Gustavo Adrian Murillo Espinosa - 317011375

;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; parser.rkt
;; ============================================================

;; parse : s-expression -> FWAE
;; Recibe una s-expression y construye el ASA correspondiente.
;; Si la expresión no pertenece al lenguaje WAE+, debe lanzar:
;;   "Syntax Error: expresion mal formada en parse"
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(boolean? sexp) (bool sexp)]
    [(symbol? sexp)
     (if (es-palabra-reservada? sexp)
         (error-sintactico)
         (id sexp))]
    [(list? sexp)
     (cond
       [(empty? sexp) (error-sintactico)]
       
       ;; si hay operadores...
       [(and (symbol? (first sexp)) (es-operador? (first sexp)))
        (construir-op (first sexp) (map parse (rest sexp)))]
       
       ;; con with...
       [(and (symbol? (first sexp)) (symbol=? (first sexp) 'with))
        (if (and (= (length sexp) 3) (list? (second sexp)))
            (construir-with (map parse-binding (second sexp)) (third sexp))
            (error-sintactico))]
       
       ;; con with*...
       [(and (symbol? (first sexp)) (symbol=? (first sexp) 'with*))
        (if (and (= (length sexp) 3) (list? (second sexp)))
            (with* (map parse-binding (second sexp)) (parse (third sexp)))
            (error-sintactico))]
       
       [else (error-sintactico)])]
    [else (error-sintactico)]))


;; Puedes agregar funciones auxiliares debajo de las funciones
;; principales que las utilicen.
;; cool :p

;; lanza el aviso de error sintactico requerido por la práctica
(define (error-sintactico)
  (error "Syntax Error: expresion mal formada en parse"))

;; palabras reservadas
(define palabras-reservadas '(with with* + - * / modulo expt add1 sub1 = < > <= >= not))

;; valida si un simbolo es un operador
(define (es-operador? s)
  (if (member s '(+ - * / modulo expt add1 sub1 = < > <= >= not)) #t #f))

;; verifica si un simbolo es una palabra reservada del sistema
(define (es-palabra-reservada? s)
  (if (member s palabras-reservadas) #t #f))

;; mapea el simbolo de la sintaxis concreta al procedimiento de Racket
(define (obtener-procedimiento op)
  (cond
    [(symbol=? op '+) +]
    [(symbol=? op '-) -]
    [(symbol=? op '*) *]
    [(symbol=? op '/) /]
    [(symbol=? op 'modulo) modulo]
    [(symbol=? op 'expt) expt]
    [(symbol=? op 'add1) add1]
    [(symbol=? op 'sub1) sub1]
    [(symbol=? op '=) =]
    [(symbol=? op '<) <]
    [(symbol=? op '>) >]
    [(symbol=? op '<=) <=]
    [(symbol=? op '>=) >=]
    [(symbol=? op 'not) not]
    [else (error-sintactico)]))

;; verifica la aridad de cada operador antes de construir el ASA
(define (verificar-aridad op args)
  (cond
    [(member op '(add1 sub1 not))
     (if (= (length args) 1) #t (error-sintactico))]
    [(member op '(modulo expt = < > <= >=))
     (if (= (length args) 2) #t (error-sintactico))]
    [(member op '(+ - * /))
     (if (>= (length args) 2) #t (error-sintactico))]
    [else (error-sintactico)]))

;; verifica si existen identificadores repetidos en una lista
(define (hay-repetidos? lst)
  (cond
    [(empty? lst) #f]
    [(member (first lst) (rest lst)) #t]
    [else (hay-repetidos? (rest lst))]))

;; valida que la estructura del binding sea [id expr], que el id sea valido y parsea su valor
(define (parse-binding b)
  (if (and (list? b)
           (= (length b) 2)
           (symbol? (first b))
           (not (es-palabra-reservada? (first b))))
      (binding (first b) (parse (second b)))
      (error-sintactico)))

;; valida la aridad y retorna la construccion del nodo op
(define (construir-op cabeza args)
  (if (verificar-aridad cabeza args)
      (op (obtener-procedimiento cabeza) args)
      (error-sintactico)))

;; protege la creacion del nodo con with asegurando que no existan identificadores duplicados
(define (construir-with bindings cuerpo)
  (if (hay-repetidos? (map binding-id bindings))
      (error-sintactico)
      (with bindings (parse cuerpo))))

;; Mas PRUEBAS!!

;;rateado de test, quitenlo si quieren, aunque me convence mas dejarlo
(print-only-errors #t)

;; validaciones de operadores y anidamiento correcto
(test (parse '{= {<= 10 20} {not {= 5 4}}})
      (op = (list (op <= (list (num 10) (num 20))) 
                  (op not (list (op = (list (num 5) (num 4))))))))

(test (parse '{/ {* 2 3 4} {- 10 2}})
      (op / (list (op * (list (num 2) (num 3) (num 4))) 
                  (op - (list (num 10) (num 2))))))

;; estructura de aridades
(test/exn (parse '{modulo 10 3 2}) 
          "Syntax Error: expresion mal formada en parse")

(test/exn (parse '{expt 2}) 
          "Syntax Error: expresion mal formada en parse")

(test/exn (parse '{= 5}) 
          "Syntax Error: expresion mal formada en parse")

;; intentos de violar restricciones de variables locales en with
(test/exn (parse '{with {{with 5}} 10}) 
          "Syntax Error: expresion mal formada en parse")

(test/exn (parse '{with* {{* 5}} 10}) 
          "Syntax Error: expresion mal formada en parse")

(test/exn (parse '{with {{x 1} {y 2} {z 3} {y 4}} x}) 
          "Syntax Error: expresion mal formada en parse")