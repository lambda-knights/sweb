# Sobre este archivo

Este archivo contiene fragmentos de código un poco más completos.

Primero se define el procedimiento `map` y `filter`:

<<Definición de `map` y `filter`>>=
(define (map proc lis)
  (if (null? lis)
      '()
      (cons (proc (car lis))
            (map proc (cdr lis)))))
            
(define (filter pred lis)
  (cond ((null? lis)
         '())
        ((pred (car lis))
         (cons (car lis)
               (filter pred (cdr lis))))
        (else
         (filter pred (cdr lis)))))
@ %def map filter

Ahora se define la lista `flow` con los primeros 100 números naturales elevados al cuadrado que sean múltiplos de 5:

<<Definición de una lista con flow>>=
(define flow (filter (lambda (n) (divides? 5 n))
                     (map square (iota 100))))
@ %def flow

Y por último nos referimos a estos dos fragmentos en un fragmento de archivo fuente:

<<mucho-flow.scm>>=
;; Primero se definen un par de procedimientos
<<Definición de `map` y `filter`>>

;; Luego se define una lista con flow
<<Definición de una lista con flow>>
@

Esto es texto mequetrefe al final.
