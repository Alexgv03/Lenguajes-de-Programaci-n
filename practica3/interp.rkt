#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Integrantes del equipo
;; Irving Axel Tapia Anrubio - 425019526
;; Brayan Alexis Gallardo Valdez - 322099449
;; Gustavo Adrian Murillo Espinosa - 317011375

;; ============================================================
;; Lenguajes de Programación 2027-1
;; Práctica 3 - Lenguaje WAE+
;; interp.rkt
;; ============================================================

;; subst : FWAE symbol FWAE -> FWAE
;; Realiza la sustitución expr[sub-id := value].
;; Debe sustituir únicamente apariciones libres y respetar el
;; alcance y sombreado de identificadores en with y with*.
(define (subst expr sub-id value)
  (type-case FWAE expr
    [id (i) (if (symbol=? i sub-id) value expr)]
    [num (n) expr]
    [bool (b) expr]
    [op (f args)
        (op f (map (λ (a) (subst a sub-id value)) args))]
    [with (bindings body)
          (with (map (λ (b) (subst-binding b sub-id value)) bindings)
                (if (declara? bindings sub-id)
                    body
                    (subst body sub-id value)))]
    [with* (bindings body)
           (subst-with* bindings body sub-id value)]))


;; interp : FWAE -> (or/c number? boolean?)
;; Evalúa una expresión WAE+ con alcance estático y evaluación
;; glotona. Para with y with* debe utilizar subst.
(define (interp expr)
  (type-case FWAE expr
    ;; para rellenar la variable libre de forma exacta
    [id (i) (error 'interp "Variable libre: ~a" i)] 
    [num (n) n]
    [bool (b) b]
    [op (f args)
        (apply f (map interp args))]
    
    ;; con with...
    [with (bindings body)
          (evaluar-y-sustituir bindings body '() '())]
    
    ;; con with*...
    [with* (bindings body)
           (if (empty? bindings)
               (interp body)
               (interp-with*-recursivo (first bindings) (rest bindings) body))]))
 

;; Puedes agregar funciones auxiliares debajo de las funciones
;; principales que las utilicen.

;;  Aux de subst

;; subst-binding : Binding symbol FWAE -> Binding
;; sustituye en el valor del binding; el id no se toca
(define (subst-binding b sub-id value)
  (binding (binding-id b) (subst (binding-value b) sub-id value)))

;; declara? : (listof Binding) symbol -> boolean
;; indica si algun binding de la lista declara a sub-id
(define (declara? bindings sub-id)
  (if (member sub-id (map binding-id bindings)) #t #f))

;; subst-with* : (listof Binding) FWAE symbol FWAE -> FWAE
;; inicia la sustitucion de with*
(define (subst-with* bindings body sub-id value)
  (recorrer-subst-with* bindings '() body sub-id value))

;; recorrer-subst-with* : (listof Binding) (listof Binding) FWAE symbol FWAE -> FWAE
;; recorre los bindings de izquierda a derecha buscando sombreado
(define (recorrer-subst-with* pendientes procesados body sub-id value)
  (cond
    [(empty? pendientes)
     (with* (reverse procesados) (subst body sub-id value))]
    [else
     (procesar-paso-subst-with* (first pendientes) (rest pendientes) procesados body sub-id value)]))

;; procesar-paso-subst-with* : Binding (listof Binding) (listof Binding) FWAE symbol FWAE -> FWAE
;; hace el paso de sustitucion en la lista de bindings
(define (procesar-paso-subst-with* b-actual resto-pendientes procesados body sub-id value)
  (if (symbol=? (binding-id b-actual) sub-id)
      (with* (append (reverse (cons (subst-binding b-actual sub-id value) procesados))
                     resto-pendientes)
             body)
      (recorrer-subst-with* resto-pendientes 
                             (cons (subst-binding b-actual sub-id value) procesados) 
                             body 
                             sub-id 
                             value)))


;; Aux de interp

;; evaluar-y-sustituir : (listof Binding) FWAE (listof symbol) (listof FWAE) -> (or/c number? boolean?)
;; recopila y evalua recursivamente los identificadores y valores externos
(define (evaluar-y-sustituir pendientes cuerpo ids-acumulados valores-acumulados)
  (if (empty? pendientes)
      (interp (sustituir-simultaneo cuerpo ids-acumulados valores-acumulados))
      (procesar-paso-with (first pendientes) (rest pendientes) cuerpo ids-acumulados valores-acumulados)))

;; procesar-paso-with : Binding (listof Binding) FWAE (listof symbol) (listof FWAE) -> (or/c number? boolean?)
;; evalua el contenido del binding actual y acumula el resultado

(define (procesar-paso-with b-actual resto-pendientes cuerpo ids-acumulados valores-acumulados)
  (evaluar-y-sustituir resto-pendientes
                       cuerpo
                       (cons (binding-id b-actual) ids-acumulados)
                       (cons (empaquetar-valor-fwae (interp (binding-value b-actual))) valores-acumulados)))

;; empaquetar-valor-fwae : (or/c number? boolean?) -> FWAE
;; envuelve un valor primitivo de racket a su nodo ast
(define (empaquetar-valor-fwae v)
  (if (number? v) (num v) (bool v)))

;; sustituir-simultaneo : FWAE (listof symbol) (listof FWAE) -> FWAE
;; inyecta los valores en el cuerpo evitando colisiones simultaneas
(define (sustituir-simultaneo cuerpo ids valores)
  (if (empty? ids)
      cuerpo
      (sustituir-simultaneo (subst cuerpo (first ids) (first valores))
                            (rest ids)
                            (rest valores))))

;; interp-with*-recursivo : Binding (listof Binding) FWAE -> (or/c number? boolean?)
;; procesa recursivamente la cabeza de un with*
(define (interp-with*-recursivo b-actual resto-bindings body)
  (interp (subst (with* resto-bindings body) 
                 (binding-id b-actual) 
                 (empaquetar-valor-fwae (interp (binding-value b-actual))))))


;; pruebas


;; operaciones aritmeticas y relacionales
(test (interp (parse '{not {>= {modulo 10 3} 5}})) #t)
(test (interp (parse '{+ {modulo 16 5} {expt 3 3}})) 28)
(test (interp (parse '{sub1 {add1 {sub1 0}}})) -1)

;; el cuerpo suma la x interna (3) mas la y calculada (10)
(test (interp (parse '{with {{x 10}}
                        {with {{x 3} {y x}}
                          {+ x y}}})) 
      13)


;; y intenta leer a x dentro de la misma lista de declaraciones
(test/exn (interp (parse '{with {{x 5} {y {+ x 1}}} y})) 
          "interp: Variable libre: x")

;; cada asignacion de x pisa y sombrea a la anterior
(test (interp (parse '{with* {{x 1} {x {+ x 2}} {x {* x 4}}} x})) 
      12)

;; variables libres en expresiones compuestas
(test/exn (interp (parse '{+ 1 2 {* 3 incognita}})) 
          "interp: Variable libre: incognita")