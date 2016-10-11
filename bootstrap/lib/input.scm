#|
Una entrada (*input*) se define como:

    input := input-null
           | (input-cons x input)

Internamente una entrada se implementa como un flujo (*stream*)
pero todo código que haga uso de esta implementación deberá
restringirse a usar las siguientes formas y procedimientos:

`input-cons`
 : Constructor de entradas

`input-null?`
 : Predicado para la entrada vacía `input-null`.

`input?`
 : Predicado para las entradas.

`input-car`
 : Selector del primer elemento de una entrada.

`input-cdr`
 : Selector de una entrada sin el primer elemento.

`open-input-from-file`
 : Procedimiento para crear una entrada a partir del nombre de un archivo.

`open-input-from-string`
 : Procedimiento para crear una entrada a partir de una cadena de caracteres.
|#

;;; Algunas implementaciones de Scheme no proveen una implementación de flujos
;;; sin embargo, desde R5RS el transformador syntax-rules y los mecanismos de
;;; delay/force son obligatorios. A continuación se implementan los flujos de
;;; manera portable.
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream element stream)
     (cons element (delay stream)))))

(define stream-null '())

(define (stream-null? x)
  (null? x))

(define (stream? x)
  (or (stream-null? x)
      (pair? x)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;;; A continuación la implementación de entradas basadas en flujos
(define-syntax input-cons
  (syntax-rules ()
    ((input-cons x in)
     (cons-stream x in))))

(define input-null stream-null)

(define (input-null? x)
  (stream-null? x))

(define (input? x)
  (or (input-null? x)
      (and (pair? x) (promise? (cdr x)))))

(define (input-car in)
  (stream-car in))

(define (input-cdr in)
  (stream-cdr in))

(define (open-input-from-file filename)
  (define ip (open-input-file filename))
  (%input-port->input% ip))

(define (open-input-from-string str)
  (define ip (open-input-string str))
  (%input-port->input% ip))

(define (%input-port->input% ip)
  (let recur ((ch (read-char ip)))
    (cond ((eof-object? ch)
           (close-input-port ip)
           input-null)
          (else
           (input-cons ch (recur (read-char ip)))))))
