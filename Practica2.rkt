#lang Racket

;; Integrantes del equipo
;; Irving Axel Tapia Anrubio - 425019526
;; Brayan Alexis Gallardo Valdez - 322099449
;; Gustavo Adrian Murillo Espinosa - 317011375

;; Ejercicio 1
;; filtra-rango: number number (listof number ) - > ( listof number )
;; Recibe dos numeros a y b, y una lista de numeros lst. Devuelve una lista
;; que contiene unicamente los elementos que pertenecen al intervalo cerrado [a, b].

;; no siento que haya mucho que comentar porque siento que lo dice todo a simple vista muejeje
(define(filtra-rango a b lst)
  (filter (λ (x) (and (>= x a) (<= x b))) lst))

;; Para ver que amarre:
;;(filtra-rango 3 8 '(1 3 5 9 8 2))
;; Debe dar '(3 5 8)
;;(filtra-rango -2 2 '(-5 -2 0 2 7))
;; Debe dar '(-2 0 2)

;; Ejercicio 2
;; cuenta-si: (A -> boolean) (listof A) -> integer
;; Recibe un predicado p? y una lista lst, y devuelve la cantidad de elementos
;; de la lista que satisfacen el predicado.

(define(cuenta-si p? lst)
  ;;el foldl lo usamos para que se sume 1 al acumuludor cada vez que el predicado se cumpla
  (foldl(λ(x acc) (if(p? x) (+ 1 acc) acc)) 0 lst))

;; Las pruebas:
;;(cuenta-si even? '(1 2 3 4 6))
;; Debe dar 3, ya que 2, 4 y 6 son pares
;;(cuenta-si (λ (x) (> x 5)) '(2 8 1 10 5))
;; debe de dar 2, ya que 8 y 10 son mayores estrictos que 5
;; (cuenta-si string? '(1 "hola" #t "racket"))
;; debe de dar 2, puesto que solo hay 2 Strings

;;Ejercicio 3
;; suma-transformados: (number -> number) (listof number) -> number
;; Recibe una funcion f y una lista de numeros lst. La funcion debera aplicar
;; f a cada elemento de la lista y devolver la suma de todos los resultados.
;; La lista vacıa debera producir como resultado 0.

(define(suma-transformados f lst)
  ;; con foldl iteramos sobre la lista, λ toma cada numero, le aplica f y suma ese resultado al acumulador. Y en caso de que la lista sea vacia regresara 0
  (foldl (λ (x acc) (+ (f x) acc)) 0 lst))

;; Pruebas:
;; (suma-transformados(λ (x) (* x x)) '(1 2 3))
;; Debe de dar 14, 1ro les saca el cuadrado a cada elemento de la lista y luego los suma
;; (suma-transformados add1 '(1 2 3))
;; Debe de dar 9, 1ro les agrega 1 a cada elemento de la lista y los suma
;; (suma-transformados (λ (x) (* x 2)) '())
;; Da 0 al ser una lista vacia


;;Parte de datos abstractos:

(struct jugador (nombre dorsal posicion) #:transparent)
;; Ejemplo:
;; (define memo (jugador 'memo 9 'delantero))
;; (define ana (jugador 'ana 10 'medio))
;; (define luis (jugador 'luis 1 'portero))

(struct equipo (nombre jugadores) #:transparent)
;; Ejemplo:
;; (define equipo-rojo (equipo 'rojos(list(jugador 'memo 9 'delantero) (jugador 'ana 10 'medio) (jugador 'luis 1 'portero))))

(struct evento (tipo equipo jugador minuto) #:transparent)
;; Ejemplo:
;; (define gol-ejemplo(evento 'gol 'local 'memo 25))

;; ATENCION: Para probar las siguientes funciones, marcare cada paso con una letra (Desde A hasta G) por si te interesa :p

;;A: definimos los equipos
;; (define eq-rojo (equipo 'rojos (list (jugador 'memo 9 'delantero) (jugador 'ana 10 'medio) (jugador 'luis 1 'portero))))
;; (define eq-azul (equipo 'azules (list (jugador 'pedro 11 'delantero) (jugador 'maria 3 'defensa))))

;; Funcion 4
;; Se define en clase una estructura llamada partido que represente el estado de un partido de futbol
;; con los campos:
;; 1.- local: Equipo que juega como local.
;; 2.- visitante: Equipo que juega como visitante.
;; 3.- goles-local: Cantidad de goles anotados por el equipo local.
;; 4.- goles-visitante: Cantidad de goles anotados por el equipo visitante.
;; 5.- minuto: Minuto actual del partido.
;; 6.- eventos: Historial de eventos ocurridos durante el partido.
;; 7.- finalizado: Booleano que indica si el partido ha terminado.

(struct partido 
  (local visitante goles-local goles-visitante minuto eventos finalizado) 
  #:transparent)

;; Funcion 5
;; Se define en clase un constructor llamado crear-partido que reciba dos equipos y produzca un
;; nuevo partido con las siguientes caracterısticas:
;; 1.- El marcador inicial es 0 a 0.
;; 2.- El minuto inicial es 0.
;; 3.- El historial de eventos esta vacıo.
;; 4.- El partido todavıa no ha finalizado.
;; 5.- Si alguno de los dos equipos no tiene jugadores, debera lanzar un error.

;; crear-partido: equipo equipo -> partido
;; asegura que las listas de jugadores de ambos equipos no sea vacio antes de generar el estado inicial
(define (crear-partido local visitante)
  (if (or (empty? (equipo-jugadores local))
          (empty? (equipo-jugadores visitante)))
      (error 'crear-partido "Alguno de los equipos no tiene jugadores")
      (partido local visitante 0 0 0 '() #f)))

;; B: Creamos el partido inicial (Marcador 0-0 y minuto 0)
;; (define partido1 (crear-partido eq-rojo eq-azul)) (partido-goles-local partido1) (partido-minuto partido1)
;; debe devolver 0 y 0

;; Ejercicio 6
;; Define una funcion llamada avanzar-minuto que reciba un partido y una cantidad de
;; minutos, y devuelva un nuevo estado del partido con el tiempo actualizado.
;; Con las reglas:
;; 1.- La cantidad de minutos que se desea avanzar debe ser positiva.
;; 2.- Si el nuevo minuto es menor que 90, el partido continua normalmente.
;; 3.- Si el nuevo minuto es mayor o igual que 90, el minuto se establece en 90 y el partido se marca
;; como finalizado.
;; 4.- Cuando el partido finalice, debera agregarse al historial un evento de tipo 'fin-partido.
;; 5.- Si el partido ya esta finalizado, debera lanzar un error.
;; Para el evento de finalizacion utiliza los sımbolos ’ninguno tanto para el equipo como para el
;; jugador.

;; avanzar-minuto: partido natural -> partido
;; Tampoco siento tan necesario explicar esta parte de los codigos ya que puse las instrucciones de que se nos pidio
(define (avanzar-minuto p minutos)
  (cond
    [(partido-finalizado p) (error 'avanzar-minuto "El partido ya finalizo")]
    [(<= minutos 0) (error 'avanzar-minuto "Los minutos a avanzar deben ser positivos")]
    [else
     (let ([nuevo-minuto (+ (partido-minuto p) minutos)])
       (if (>= nuevo-minuto 90)
           (partido (partido-local p)
                    (partido-visitante p)
                    (partido-goles-local p)
                    (partido-goles-visitante p)
                    90
                    (cons (evento 'fin-partido 'ninguno 'ninguno 90) (partido-eventos p))
                    #t)
           (partido (partido-local p)
                    (partido-visitante p)
                    (partido-goles-local p)
                    (partido-goles-visitante p)
                    nuevo-minuto
                    (partido-eventos p)
                    #f)))]))

;; funcioncilla auxiliar pa' verificar si un jugador esta en un equipo
(define (jugador-pertenece? nombre lst-jugadores)
  (not (empty? (filter (λ (j) (symbol=? (jugador-nombre j) nombre)) lst-jugadores))))

;; C: Avanzamos el tiempo 25 minutotes
;; (define partido2 (avanzar-minuto partido1 25))(partido-minuto partido2) (partido-finalizado partido2)
;; Debe de devolver 25 y #f

;; Ejercicio 7
;; Define una funcion llamada registrar-gol que reciba:
;; 1.- Un partido.
;; 2.- Un sımbolo que indique el equipo que anoto: 'local o 'visitante.
;; 3.- Un sımbolo con el nombre del jugador que anoto.
;; La funcion debera devolver un nuevo estado del partido con:
;; 1.- El marcador correspondiente incrementado en uno.
;; 2.- Un nuevo evento de tipo 'gol agregado al historial.
;; 3.- El minuto actual del partido registrado en el evento.
;; Ademas, debera verificar lo siguiente:
;; 1.- El partido no debe estar finalizado.
;; 2.- El sımbolo del equipo debe ser 'local o 'visitante.
;; 3.- El jugador debe pertenecer al equipo que anoto.
;; En caso de que alguna condicion no se cumpla, deber ́a lanzar un error.

;; registrar-gol: symbol symbol -> partido
(define (registrar-gol p lado nombre-jugador)
  (cond
    [(partido-finalizado p) 
     (error 'registrar-gol "El partido ya finalizó")]
    [(not (or (symbol=? lado 'local) (symbol=? lado 'visitante)))
     (error 'registrar-gol "El lado debe ser 'local o 'visitante")]
    [(and (symbol=? lado 'local) 
          (not (jugador-pertenece? nombre-jugador (equipo-jugadores (partido-local p)))))
     (error 'registrar-gol "El jugador no pertenece al equipo local")]
    [(and (symbol=? lado 'visitante) 
          (not (jugador-pertenece? nombre-jugador (equipo-jugadores (partido-visitante p)))))
     (error 'registrar-gol "El jugador no pertenece al equipo visitante")]
    [else
     (let ([nuevo-evento (evento 'gol lado nombre-jugador (partido-minuto p))])
       (if (symbol=? lado 'local)
           (partido (partido-local p)
                    (partido-visitante p)
                    (+ 1 (partido-goles-local p))
                    (partido-goles-visitante p)
                    (partido-minuto p)
                    (cons nuevo-evento (partido-eventos p))
                    (partido-finalizado p))
           (partido (partido-local p)
                    (partido-visitante p)
                    (partido-goles-local p)
                    (+ 1 (partido-goles-visitante p))
                    (partido-minuto p)
                    (cons nuevo-evento (partido-eventos p))
                    (partido-finalizado p))))]))

;; D: registramos un gol al equipo local
;; (define partido3 (registrar-gol partido2 'local 'memo)) (partido-goles-local partido3) (first (partido-eventos partido3))
;; debe devolver  1  y el evento de gol


;; Ejercicio 8
;; Define una funcion llamada buscar-jugadores que reciba un partido y un predicado de
;; un argumento. 
;; La funcion debera aplicar el predicado a todos los jugadores de ambos equipos y devolver una lista
;; con aquellos que lo satisfagan.

;; buscar-jugadores: partido (jugador -> boolean) -> (listof jugador)
(define (buscar-jugadores p pred)
  (filter pred (append (equipo-jugadores (partido-local p))
                       (equipo-jugadores (partido-visitante p)))))

;;  E: Buscar jugadores
;; (buscar-jugadores partido1 (lambda (j) (symbol=? (jugador-posicion j) 'delantero)))
;; debe devolver la lista con memo y pedro


;; decimas etztras (por favor diganme que no nos bajan puntos por mis "errores ortograficos", son intencionales ;(. Exceptuando los acentos, esos no puedo ponerlos xd)

;; Ejercicio 9
;; Define una funcion llamada resultado-partido que reciba un partido finalizado y devuelva
;; alguno de los siguientes sımbolos:
;; - 'gana-local, si el equipo local anoto mas goles.
;; - 'gana-visitante, si el equipo visitante anoto mas goles.
;; - 'empate, si ambos equipos anotaron la misma cantidad de goles.
;; Si el partido todavıa no ha terminado, debera lanzar un error.

;; resultado-partido: partido -> symbol
(define (resultado-partido p)
  (cond
    [(not (partido-finalizado p)) (error 'resultado-partido "El partido no ha terminado")]
    [(> (partido-goles-local p) (partido-goles-visitante p)) 'gana-local]
    [(< (partido-goles-local p) (partido-goles-visitante p)) 'gana-visitante]
    [else 'empate]))

;; F: culminamos el partido (decidimos avanzarle 70 minutotes para que se vea que si topa con 90)
;; (define partido-final (avanzar-minuto partido3 70)) (partido-minuto partido-final) (partido-finalizado partido-final)
;; debe de devolver los 90 minutillos y #t de que e' verda' que termino


;; G: Y para el resultado...
;; (resultado-partido partido-final)
;; debe decir que gano local

;; Ejercicio 10
;; Define una funcion que pruebe la implementacion completa del partido:
;; -Crea los jugadores y los equipos.
;; -Inicializa el partido.
;; -Avanza el tiempo.
;; -Registra varios goles.
;; -Finaliza el partido.
;; -Obtiene el resultado.

;; prueba-completa: -> symbol

(define (prueba-completa)
  (let* ([j1 (jugador 'memo 9 'delantero)]
         [j2 (jugador 'ana 10 'medio)]
         [j3 (jugador 'luis 1 'portero)]
         [j4 (jugador 'pedro 11 'delantero)]
         [j5 (jugador 'maria 3 'defensa)]
         [eq-rojo (equipo 'rojos (list j1 j2 j3))]
         [eq-azul (equipo 'azules (list j4 j5))]
         ;; empieza el partido
         [p-inicial (crear-partido eq-rojo eq-azul)]
         ;; avanza el tiempo a 25 min y anota local
         [p-min25 (avanzar-minuto p-inicial 25)]
         [p-gol1 (registrar-gol p-min25 'local 'memo)]
         ;; avanza el tiempo a 45 min y anota visitante
         [p-min45 (avanzar-minuto p-gol1 20)]
         [p-gol2 (registrar-gol p-min45 'visitante 'pedro)]
         ;; avanza el tiempo a 95 min (el sistema topa en 90 y fin)
         [p-final (avanzar-minuto p-gol2 50)])
    (resultado-partido p-final)))

;; Para ejecutar:
;; (prueba-completa
;; debe regresar 'empate