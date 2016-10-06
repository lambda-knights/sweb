#|

# (`lib/parcomb.scm`)

## ***Documentación***

Un parser es un procedimiento que toma tres argumentos:

1. Una entrada `in`
2. Una continuación `ok`
3. Una continuación `bad`

Las *continuaciones* `ok` y `bad` son procedimientos que reciben un resultado de parseo y realizan algún cómputo con él.

Hay dos tipos de resultado de parseo:

- Éxitos
- Fallos

Las continuaciones `ok` reciben un resultado de éxito `s` mientras que las continuaciones `bad` reciben un resultado de fallo `f`.

Un resultado de éxito se puede construír con el procedimiento `success` el cuál toma dos argumentos:

1. Un objeto cualquiera `match` que corresponde la información que fue parseada
2. Una entrada `pending` que corresponde al resto de la entrada que no fue consumida

Un resultado de fallo se puede construír con el procedimiento `failure` el cuál toma dos argumentos:

1. Un objeto cualquiera `why` que corresponde a la información de fallo
2. Una entrada `pending-input` que corresponde a la entrada que no pudo ser parseada

Los procedimientos `success-match`, `why-failed` y `pending-input` obtienen el `match` el `why` y el `pending` de los resultados.

Los combinadores de parsers son procedimientos que toman como argumentos a parsers y regresan un parser.

|#

#|

## ***Implementación***

|#

#|

### ***Parsers***

`:succeed:`
 : siempre parsea la entrada correctamente sin consumirla y el *match* satisface `null?`

`:fail:`
 : siempre parsea la entrada incorrectamente sin consumirla y el *why* es la cadena `"no reason"`

`:empty:`
 : parsea correctamente solo las entradas vacías

`:nempty`
 : parsea correctamente solo las entradas no vacías

`(:pred: pred)`
 : parsea correctamente si el primer elemento de una entrada satisface `pred`

`(:npred: pred)`
 : similar a `:pred:` pero con el predicado negado

`((:cmp: =) x)`
 : produce creadores de parsers que comparan el primer elemento de una entrada con `x` bajo la relación `=`

`((:ncmp: =) x)`
 : similar a `:cmp:` pero negando la relación `=`

`(:equal: x)`
 : equivalente a `(:cmp: equal?)`

`(:eqv: x)`
 : equivalente a `(:cmp: eqv?)`

`(:eq: x)`
 : equivalente a `(:cmp: eq?)`

`(:nequal: x)`
 : equivalente a `(:ncmp: equal?)`

`(:neqv: x)`
 : equivalente a `(:ncmp: eqv?)`

`(:neq: x)`
 : equivalente a `(:ncmp: eq?)`


|#

(define (:succeed: in ok bad)
  (ok (success '() in)))
#||#

(define (:fail: in ok bad)
  (bad (failure '() in)))
#||#

(define (:empty: in ok bad)
  (if (input-null? in)
      (:succeed: in ok bad)
      (bad (failure (list (list "not an empty input"
                                (list (cons "found:" (input-car in))))) in))))
#||#

(define (:nempty: in ok bad)
  (if (input-null? in)
      (bad (failure '(("not expecting empty input")) in))
      (:succeed: in ok bad)))
#||#

(define ((:pred: pred) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((pred (input-car in))
         (ok (success (list (input-car in))
                      (input-cdr in))))
        (else
         (bad (failure (list (list "(pred x) => #f"
                                   (list (cons "x is:" (input-car in))
                                         (cons "pred is:" pred))))
                       in)))))
#||#

(define ((:npred: pred) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((pred (input-car in))
         (bad (failure (list (list "(pred x) => #t"
                                   (list (cons "x is:" (input-car in))
                                         (cons "pred is:" pred)))))))
        (else
         (ok (success (list (input-car in))
                      (input-cdr in))))))
#||#

(define (((:cmp: =) x) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((= x (input-car in))
         (ok (success (list (input-car in))
                      (input-cdr in))))
        (else
         (bad (failure (list (list "x ≠ y"
                                   (list (cons "x is:" x)
                                         (cons "y is:" (input-car in))
                                         (cons "= is:" =))))
                       in)))))
#||#

(define :equal: (:cmp: equal?))
(define :eqv: (:cmp: eqv?))
(define :eq: (:cmp: eq?))
#||#

(define (((:ncmp: =) x) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((= x (input-car in))
         (bad (failure (list (list "x = y"
                                   (list (cons "x is:" x)
                                         (cons "y is:" (input-car in))
                                         (cons "= is:" =))))
                       in)))
        (else
         (ok (success (list (input-car in))
                      (input-cdr in))))))
#||#

(define :nequal: (:ncmp: equal?))
(define :neqv: (:ncmp: eqv?))
(define :neq: (:ncmp: eq?))
#||#

#|

### ***Combinadores básicos***

`(:seq: pars ...)`
 : Parsea de manera secuencial la entrada utilizando sus argumentos en orden, corresponde a una concatenación o secuenciación de parsers

`(:alt: pars ...)`
 : Parsea de manera condicional la entrada utilizando sus argumentos en orden, corresponde a una disyunción o alternativa de parsers y deja de parsear cuando uno de sus parsers es un éxito

`(:*: par)`
 : Parsea cero o más veces la entrada utilizando el parser `par`

`(:+: par)`
 : Parsea una o más veces la entrada utilizando el parser `par`


|#

(define (:seq: . pars)
  (define ((:seq2: par1 par2) in ok bad)
    (par1 in
          (lambda (s1)
            (par2 (pending-input s1)
                  (lambda (s2)
                    (ok (success (append (success-match s1)
                                         (success-match s2))
                                 (pending-input s2))))
                  (lambda (f2)
                    (bad f2))))
          bad))
  (fold-right :seq2: :succeed: pars))
#||#

(define (:alt: . pars)
  (define ((:alt2: par1 par2) in ok bad)
    (par1 in
          ok
          (lambda (f1)
            (par2 in
                  ok
                  (lambda (f2)
                    (bad (failure (append (why-failed f1)
                                          (why-failed f2))
                                  in)))))))
  (fold-right :alt2: :fail: pars))
#||#

(define (:*: par)
  (define (par* in ok bad)
    (par in
         (lambda (s1)
           (let ((s2 (par* (pending-input s1) id id)))
             (ok (success (append (success-match s1)
                                  (success-match s2))
                          (pending-input s2)))))
         (lambda (f1)
           (:succeed: in ok bad))))
  par*)
#||#

(define (:+: par)
  (:seq: par (:*: par)))
#||#

#|

### ***Metacombinadores***

`((:><: fmatch fwhy) par)`
 : Regresa un combinador que utiliza `par` sobre la entrada y aplica `fmatch` al `match` de un éxito y `fwhy` al `why` de un fallo.

|#

(define (((:><: fmatch fwhy) par) in ok bad)
  (par in
       (lambda (s)
         (ok (success (fmatch (success-match s))
                      (pending-input s))))
       (lambda (f)
         (bad (failure (fwhy (why-failed f))
                       (pending-input f))))))
#||#

#|

### ***Resultados de parseo***

|#

(define (success match pending)
  `(success ,match ,pending))

(define (success? x)
  (and (list? x)
       (= 3 (length x))
       (eq? 'success (car x))))
#||#

(define (failure why pending)
  `(failure ,why ,pending))

(define (failure? x)
  (and (list? x)
       (= 3 (length x))
       (eq? 'failure (car x))))
#||#

(define (success-match s)
  (assert (success? s))
  (cadr s))

(define (why-failed f)
  (assert (failure? f))
  (cadr f))

(define (pending-input r)
  (assert (or (success? r) (failure? r)))
  (caddr r))
#||#

#|

### ***Macros***

Se utilizan los macros `parser` y `define-parser` para admitir la definición recursiva de parsers. Si estas formas no son utilizadas se pueden encontrar errores, considera:

`> (define foo (:alt: :empty: (:seq: (:eqv: 1) foo)))`

|#

(define-syntax parser
  (syntax-rules ()
    ((parser par)
     (lambda (in ok bad)
       (par in ok bad)))))

(define-syntax define-parser
  (syntax-rules ()
    ((define-parser name par)
     (define name (parser par)))))
#||#
