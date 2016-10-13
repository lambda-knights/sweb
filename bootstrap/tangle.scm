;;; Required libraries
(load "lib/utils.scm")
(load "lib/input.scm")
(load "lib/parcomb.scm")

;;; Main procedures
(define (open-sweb-file ifilename)
  (make-sweb (sweb-parse (sweb-tokenize (open-input-from-file ifilename)))
             (filename-remove-extension ifilename)))

(define (stangle-write sweb changes)
  (sweb-write (sweb-apply-changes changes sweb)))

;;; Tokenization
(define-syntax-rule (define-token name str)
  (define name (string->symbol str)))

(define-token tok-bar   "bar")
(define-token tok-nl    "nl")
(define-token tok-@@    "@@")
(define-token tok-@     "@ ")
(define-token tok-@*    "@*")
(define-token tok-@d    "@d")
(define-token tok-@f    "@f")
(define-token tok-@s    "@s")
(define-token tok-@c    "@c")
(define-token tok-@p    "@p")
(define-token tok-@<    "@<")
(define-token tok-@lp   "@(")
(define-token tok-@h    "@h")
(define-token tok-@^    "@^")
(define-token tok-@.    "@.")
(define-token tok-@:    "@:")
(define-token tok-@t    "@t")
(define-token tok-@=    "@=")
(define-token tok-@q    "@q")
(define-token tok-@!    "@!")
(define-token tok-@quot "@'")
(define-token tok-@&    "@&")
(define-token tok-@l    "@l")
(define-token tok-@cm   "@,")
(define-token tok-@/    "@/")
(define-token tok-@bar  "@bar")
(define-token tok-@ht   "@#")
(define-token tok-@+    "@+")
(define-token tok-@sc   "@sc")
(define-token tok-@lb   "@[")
(define-token tok-@rb   "@]")
(define-token tok-@x    "@x")
(define-token tok-@y    "@y")
(define-token tok-@z    "@z")
(define-token tok-@i    "@i")
(define-token tok-@>    "@>")
(define-token tok-@>=   "@>=")
(define-token tok-@>+=  "@>+=")

(define-syntax-rule (input-string-recognizer (str tok) (strs toks) ...)
  (make-recognizer (list (cons (string->list str) tok)
                         (cons (string->list strs) toks)
                         ...)))

(define (make-recognizer pairs)
  (let* ((pairs* (sort* (lambda (p1 p2)
                          (> (length (car p1))
                             (length (car p2))))
                        pairs))
         (inits (remove-duplicates*
                 char=? (map (lambda (p) (caar p)) pairs*))))
    (define (special? ch)
      (memv ch inits))
    (define (match-input/tokens in)
      (let loop ((pairs* pairs*))
        (cond ((null? pairs*) #f)
              ((equal? (caar pairs*) (input-take in (length (caar pairs*))))
               (car pairs*))
              (else
               (loop (cdr pairs*))))))
    (define (tokenize in)
      (if (input-null? in)
          (input-cons tok-nl input-null)
          (let ((ch (input-car in)))
            (cond ((and (special? ch)
                        (match-input/tokens in))
                   => (lambda (pair)
                        (input-cons (cdr pair)
                                    (tokenize (input-drop in (length (car pair)))))))
                  (else
                   (receive (lis in) (input-break special? (input-cdr in))
                     (input-cons (list->string (cons ch lis))
                                 (tokenize in))))))))
    (lambda (in)
      (tokenize in))))

(define sweb-tokenize
  (input-string-recognizer
   ("|"    tok-bar)
   ("\n"   tok-nl)
   ("@@"   tok-@@)
   ("@ "   tok-@)
   ("@\n"  tok-@)
   ("@*"   tok-@*)
   ("@d"   tok-@d)
   ("@f"   tok-@f)
   ("@s"   tok-@s)
   ("@c"   tok-@c)
   ("@p"   tok-@p)
   ("@<"   tok-@<)
   ("@("   tok-@lp)
   ("@h"   tok-@h)
   ("@^"   tok-@^)
   ("@."   tok-@.)
   ("@:"   tok-@:)
   ("@t"   tok-@t)
   ("@="   tok-@=)
   ("@q"   tok-@q)
   ("@!"   tok-@!)
   ("@'"   tok-@quot)
   ("@&"   tok-@&)
   ("@l"   tok-@l)
   ("@,"   tok-@cm)
   ("@/"   tok-@/)
   ("@|"   tok-@bar)
   ("@#"   tok-@ht)
   ("@+"   tok-@+)
   ("@;"   tok-@sc)
   ("@["   tok-@lb)
   ("@]"   tok-@rb)
   ("@x"   tok-@x)
   ("@y"   tok-@y)
   ("@z"   tok-@z)
   ("@i"   tok-@i)
   ("@>"   tok-@>)
   ("@>="  tok-@>=)
   ("@>+=" tok-@>+=)))

;;; Parsing
(define (sweb-parse token-lis)
  (<sweb> token-lis pp error))

(define-parser <sweb>
  (wrap-sweb
   (:seq: <limbo> (:*: <section>))))

(define-parser <limbo>
  (wrap-limbo
   (:*: (:pred: (lambda (x)
                  (not (or (eq? x tok-@)
                           (eq? x tok-@*))))))))

(define-parser <section>
  (wrap-section
   (:seq: (:alt: (:eq: tok-@) (:eq: tok-@*))
          <limbo>)))

(define (generic-wrapper tag reason)
  (:><: (lambda (match)
          `((,tag ,match)))
        (lambda (why)
          `((,reason . ,why)))))

(define wrap-sweb
  (generic-wrapper 'sweb "malformed sweb program"))

(define wrap-limbo
  (generic-wrapper 'limbo "malformed limbo"))

(define wrap-section
  (generic-wrapper 'section "malformed section"))

;;; SWEB structure
(define (make-sweb program name)
  'TODO)

;;; Changes
(define (sweb-apply-changes changes sweb)
  'TODO)

;;; Output
(define (sweb-write sweb)
  'TODO)
