(define-syntax define*
  (syntax-rules ()
    ((define* (spec . args)
       body1 body2 ...)
     (define* spec
       (lambda args
         body1 body2 ...)))
    ((define* name value)
     (define name value))))

(define-syntax assert*
  (syntax-rules ()
    ((assert* condition)
     (unless condition
       (error "Assertion failed" 'condition)))))

(define (fold-right1 kons knil lis)
  (if (null? lis)
      knil
      (kons (car lis)
            (fold-right1 kons knil (cdr lis)))))

(define (id x) x)

(define* ((constant x) y) x)

(define (negate pred)
  (lambda (x)
    (not (pred x))))

(define (compose f . fs)
  (lambda args
    (let loop ((f  f)
               (fs fs))
      (if (null? fs)
          (apply f args)
          (f (loop (car fs) (cdr fs)))))))

(define (reverse-compose f . fs)
  (lambda args
    (let loop ((res (apply f args))
               (fs  fs))
      (if (null? fs)
          res
          (loop ((car fs) res) (cdr fs))))))

(define (pipe x . fs)
  ((apply reverse-compose fs) x))
