(define (id x) x)

(define ((constant x) y) x)

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
