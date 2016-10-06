#|

# (`lib/input.scm`)

## ***Documentación***

Una entrada (*input*) se define como:

    input := input-null
           | (input-cons x input)

Internamente una entrada se implementa como un flujo (*stream*)
pero todo código que haga uso de esta implementación deberá
restringirse a usar los siguientes procedimientos:

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

#|

## ***Implementación***

|#

#|

### ***Macros***

|#

(define-syntax input-cons
  (syntax-rules ()
    ((input-cons x in)
     (cons-stream x in))))
#||#

#|

### ***Procedimientos***

|#

(define input-null (stream))
#||#

(define (input-null? x)
  (stream-null? x))
#||#

(define (input? x)
  (or (input-null? x)
      (and (pair? x) (promise? (cdr x)))))
#||#

(define (input-car in)
  (stream-car in))
#||#

(define (input-cdr in)
  (stream-cdr in))
#||#

(define (open-input-from-file filename)
  (define ip (open-input-file filename))
  (%input-port->input% ip))
#||#

(define (open-input-from-string str)
  (define ip (open-input-string str))
  (%input-port->input% ip))
#||#

#|

### ***Procedimientos internos***

(no utilizar fuera de este archivo)

|#

(define (%input-port->input% ip)
  (let recur ((ch (read-char ip)))
    (cond ((eof-object? ch)
           (close-input-port ip)
           input-null)
          (else
           (input-cons ch (recur (read-char ip)))))))
#||#
