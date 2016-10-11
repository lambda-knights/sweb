#|
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

(define (:succeed: in ok bad)
  (ok (success '() in)))

(define (:fail: in ok bad)
  (bad (failure '() in)))

(define (:empty: in ok bad)
  (if (input-null? in)
      (:succeed: in ok bad)
      (bad (failure `(("not an empty input"
                       (("found:" . ,(input-car in)))))
                    in))))

(define (:nempty: in ok bad)
  (if (input-null? in)
      (bad (failure '(("not expecting empty input")) in))
      (:succeed: in ok bad)))

(define ((:pred: pred) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((pred (input-car in))
         (ok (success (list (input-car in))
                      (input-cdr in))))
        (else
         (bad (failure `(("(pred x) => #f"
                          (("x is:" . ,(input-car in))
                           ("pred is:" . ,pred))))
                       in)))))

(define ((:npred: pred) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((pred (input-car in))
         (bad (failure `(("(pred x) => #t"
                          (("x is:" . ,(input-car in))
                           ("pred is:" . ,pred))))
                       in)))
        (else
         (ok (success (list (input-car in))
                      (input-cdr in))))))

(define (((:cmp: =) x) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((= x (input-car in))
         (ok (success (list (input-car in))
                      (input-cdr in))))
        (else
         (bad (failure `(("x ≠ y"
                          (("x is:" . ,x)
                           ("y is:" . ,(input-car in))
                           ("= is:" . ,=))))
                       in)))))

(define :equal: (:cmp: equal?))
(define :eqv: (:cmp: eqv?))
(define :eq: (:cmp: eq?))

(define (((:ncmp: =) x) in ok bad)
  (cond ((input-null? in)
         (:nempty: in ok bad))
        ((= x (input-car in))
         (bad (failure `(("x = y"
                          (("x is:" . ,x)
                           ("y is:" . ,(input-car in))
                           ("= is:" . ,=))))
                       in)))
        (else
         (ok (success (list (input-car in))
                      (input-cdr in))))))

(define :nequal: (:ncmp: equal?))
(define :neqv: (:ncmp: eqv?))
(define :neq: (:ncmp: eq?))

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

(define (:+: par)
  (:seq: par (:*: par)))

(define (((:><: fmatch fwhy) par) in ok bad)
  (par in
       (lambda (s)
         (ok (success (fmatch (success-match s))
                      (pending-input s))))
       (lambda (f)
         (bad (failure (fwhy (why-failed f))
                       (pending-input f))))))

(define (success match pending)
  (vector 'success match pending))

(define (success? x)
  (and (vector? x)
       (= 3 (vector-length x))
       (eq? 'success (vector-ref x 0))))

(define (failure why pending)
  (vector 'failure why pending))

(define (failure? x)
  (and (vector? x)
       (= 3 (vector-length x))
       (eq? 'failure (vector-ref x 0))))

(define (success-match s)
  (assert (success? s))
  (vector-ref s 1))

(define (why-failed f)
  (assert (failure? f))
  (vector-ref f 1))

(define (pending-input r)
  (assert (or (success? r) (failure? r)))
  (vector-ref r 2))

(define-syntax parser
  (syntax-rules ()
    ((parser par)
     (lambda (in ok bad)
       (par in ok bad)))))

(define-syntax define-parser
  (syntax-rules ()
    ((define-parser name par)
     (define name (parser par)))))
