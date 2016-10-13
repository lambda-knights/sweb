(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name . spec)
       transform)
     (define-syntax name
       (syntax-rules ()
         ((name . spec) transform))))))

(define-syntax define*
  (syntax-rules ()
    ((define* (spec . args)
       body1 body2 ...)
     (define* spec
       (lambda args
         body1 body2 ...)))
    ((define* name value)
     (define name value))))

(define-syntax-rule (receive args expr body1 body2 ...)
  (call-with-values (lambda () expr)
    (lambda args
      body1 body2 ...)))

(define-syntax-rule (assert* condition)
  (unless condition
    (error "Assertion failed" 'condition)))

(define (fold-right1 kons knil lis)
  (if (null? lis)
      knil
      (kons (car lis)
            (fold-right1 kons knil (cdr lis)))))

(define (split* lis)
  (let loop ((lis lis)
             (a   '())
             (b   '()))
    (cond ((null? lis)       (values a b))
          ((null? (cdr lis)) (values a (cons (car lis) b)))
          (else              (loop (cddr lis)
                                   (cons (car lis) a)
                                   (cons (cadr lis) b))))))

(define (merge* < olis1 olis2)
  (cond ((null? olis1) olis2)
        ((null? olis2) olis1)
        (else
         (let ((x (car olis1))
               (y (car olis2)))
           (if (< x y)
               (cons x (merge* < (cdr olis1) olis2))
               (cons y (merge* < olis1 (cdr olis2))))))))

(define (sort* < lis)
  (if (or (null? lis) (null? (cdr lis)))
      lis
      (call-with-values
          (lambda ()
            (split* lis))
        (lambda (lis1 lis2)
          (let ((olis1 (sort* < lis1))
                (olis2 (sort* < lis2)))
            (merge* < olis1 olis2))))))

(define (remove* pred lis)
  (cond ((null? lis) '())
        ((pred (car lis))
         (remove* pred (cdr lis)))
        (else
         (cons (car lis) (remove* pred (cdr lis))))))

(define (remove-duplicates* = lis)
  (if (null? lis)
      '()
      (cons (car lis)
            (remove-duplicates*
             = (remove* (lambda (x)
                          (= x (car lis)))
                        (cdr lis))))))

(define (filename-remove-extension filename)
  (let loop ((lis (reverse (string->list filename))))
    (cond ((null? lis)
           filename)
          ((char=? #\. (car lis))
           (list->string (reverse (cdr lis))))
          (else
           (loop (cdr lis))))))

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
