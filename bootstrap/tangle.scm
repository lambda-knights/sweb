#|
Esta es la implementación del procedimiento `tangle`, este tiene el objetivo de
tomar un archivo de entrada y un nombre de fragmento de código y extraer todos
los fragmentos de código del archivo con el nombre dado expandiendo las
referencias a otros fragmentos.

Esta extracción de código es vaciada en un archivo nombrado como el nombre del
fragmento dado.
|#

(load "lib/utils.scm")
(load "lib/input.scm")
(load "lib/parcomb.scm")

(define (tangle ifilename cname)
  (pipe #f
        (markup ifilename)
        (flatten-code-content cname)
        (dump-flat-content cname)))

;;; En esta etapa de toma una lista de fragmentos y a partir del nombre de un
;;; fragmento de código `cname` se concatena y aplana de manera recursiva el
;;; contenido de los códigos relevantes a `cname`.
(define ((flatten-code-content cname) chunks)
  (let ((code-chunks (filter code? chunks)))
    (expand-code-content-refs (append-code-content cname code-chunks)
                              code-chunks 0)))

;;; El siguiente procedimiento se encarga únicamente de concatenar el contenido de
;;; los códigos relevantes al nombre `cname`.
(define (append-code-content cname code-chunks)
  (define content
    (apply append (map code-content
                       (filter (lambda (code) (string=? cname (code-name code)))
                               code-chunks))))
  (define len (length content))
  (if (zero? len)
      (error "No chunk is named" cname)
      (drop-right content 1)))

;;; El siguiente procedimiento expande las referencias de un contenido concatenado
;;; tomando en cuenta las columnas en las que inician las referencias.
(define (expand-code-content-refs content code-chunks indent-spaces)
  (define indent-str (make-string indent-spaces #\space))
  (let recur ((content content)
              (col  0))
    (if (null? content)
        '()
        (let ((x  (car content))
              (xs (cdr content)))
          (cond ((eqv? #\newline x)
                 (cons x (cons indent-str (recur xs 0))))
                ((string? x)
                 (cons x (recur xs (+ col (string-length x)))))
                ((refs? x)
                 (let ((fill (append-code-content (refs-name x) code-chunks)))
                   (append (expand-code-content-refs fill code-chunks col)
                           (recur xs col))))
                (else
                 (error "Malformed code content" content)))))))

;;; En esta etapa se escribe a un archivo un contenido sin referencias, es decir,
;;; una lista compuesta únicamente de cadenas de caracteres o del caracter
;;; `#\newline`.
(define ((dump-flat-content ofilename) content)
  (with-output-to-file ofilename
    (lambda ()
      (for-each display content))))

;;; Esta etapa es la más complicada de todo el programa, se encarga de leer un
;;; archivo con la sintaxis de `sweb` y regresar la representación interna del
;;; programa como una lista de fragmentos.
(define ((markup ifilename) anything)
  (parse (tokenize (open-input-from-file ifilename))))

;;; Primero se tokeniza la entrada, esto se logra identificando que caracteres de la
;;; entrada conforman tokens, nuevas líneas o texto.
(define token-newline ':token-newline)
(define token-lbracks ':token-lbracks)
(define token-rbreqs  ':token-rbreqs)
(define token-rbracks ':token-rbracks)
(define token-at      ':token-at)
(define token-def     ':token-def)

(define (tokenize in)
  (if (input-null? in)
      (input-cons token-newline input-null)
      (let ((ch  (input-car in)))
        (cond ((and (special? ch)
                    (match-input/tokens in))
               => (lambda (entry)
                    (input-cons (cdr entry)
                                (tokenize (input-drop in (string-length (car entry)))))))
              (else
               (receive (lis in) (input-break special? (input-cdr in))
                 (input-cons (list->string (cons ch lis))
                             (tokenize in))))))))

(define *string-token-map*
  `(("\n"     . ,token-newline)
    ("<<"     . ,token-lbracks)
    (">>="    . ,token-rbreqs)
    (">>"     . ,token-rbracks)
    ("@<<"    . "<<")
    ("@>>"    . ">>")
    ("@@"     . "@")
    ("@%def " . "%def ")
    ("@"      . ,token-at)
    ("%def "  . ,token-def)))

(define special?
  (let ((chs (delete-duplicates (map (lambda (entry)
                                       (string-ref (car entry) 0))
                                     *string-token-map*)
                                char=?)))
    (lambda (ch)
      (member ch chs char=?))))

(define (match-input/tokens in)
  (let loop ((entries *string-token-map*))
    (if (null? entries)
        #f
        (let ((str (caar entries)))
          (if (equal? (string->list str)
                      (input-take in (string-length str)))
              (car entries)
              (loop (cdr entries)))))))

;;; Los siguientes procedimientos son algoritmos para entradas análogos a los
;;; procedimientos `take`, `drop` y `break` especificados para listas en
;;; [SRFI-1](http://srfi.schemers.org/srfi-1/srfi-1.html).
(define (input-take in n)
  (if (or (zero? n) (input-null? in))
      '()
      (cons (input-car in)
            (input-take (input-cdr in) (- n 1)))))

(define (input-drop in n)
  (if (or (zero? n) (input-null? in))
      in
      (input-drop (input-cdr in) (- n 1))))

(define (input-break pred in)
  (let loop ((nots '())
             (rest in))
    (if (or (input-null? rest)
            (pred (input-car rest)))
        (values (reverse nots) rest)
        (loop (cons (input-car rest) nots)
              (input-cdr rest)))))

#|
El parseo de los tokens se logra programando la gramática de los programas con
sintaxis de `sweb`, la especificación de la gramática con la sintaxis de
`lib/parcomb.scm` es:

```
<program> => (:+: (:alt: <docs> <code>))

<docs>    => (:alt: (:seq: (:eq: token-at)
                           (:*: (:pred: string-spaces?)))
                           (:eq: token-newline))
                           <docs-lines>)
                    <docs-lines>)

<code>    => (:seq: (:eq: token-lbracks)
                    (:*: (:pred: string?))
                    (:eq: token-rbreqs)
                    (:*: (:pred: string-spaces?))
                    (:eq: token-newline)
                    <code-lines>)

<docs-lines> => (:+: (:seq: (:*: (:alt: (:pred: string?) <refs>))
                            (:eq: token-newline)))

<code-lines> => (:alt: (:seq: (:*: (:alt: (:pred: string?) <refs>))
                              (:eq: token-newline)
                              <code-lines>)
                       (:seq: (:eq: token-at)
                              (:+: (:pred: string-spaces?))
                              (:eq: token-def)
                              (:+: (:pred: string?))
                              (:eq: token-newline))
                       :succeed:)

<refs>       => (:seq: (:eq: token-lbracks)
                       (:*: (:pred: string?))
                       (:eq: token-rbracks))
```

La implementación concreta de la gramática implementa la construcción de la
representación interna usando envolturas de los éxitos de parseo y la
construcción de un árbol de razones por las que el parseo pudo fallar usando
envolturas de las razones de fallo de parseo.
|#
(define (parse tks)
  (<program> tks
             (lambda (s)
               (if (input-null? (pending-input s))
                   (success-match s)
                   (parse (pending-input s))))
             (lambda (f)
               (display "THE PARSING PROCESS FAILED\n")
               (display "==========================\n\n")
               (display "Here is the reason tree of why it may have failed:\n")
               (dump-error-tree (why-failed f))
               (display "\nHere is a textual representation of the pending input:\n")
               (display "---------- input starts here ----------\n")
               (dump-detokenized (pending-input f))
               (display "----------- input ends here -----------\n")
               (error "Parsing failed"))))

;;; El siguiente procedimiento revierte la tokenización para facilitar la
;;; lectura del archivo que falla parsear.
(define (dump-detokenized tks)
  (unless (input-null? tks)
    (let ((tk  (input-car tks)))
      (cond ((eq? token-lbracks tk) (display "<<"))
            ((eq? token-rbreqs tk)  (display ">>="))
            ((eq? token-newline tk) (newline))
            ((eq? token-def tk)     (display "%def "))
            ((eq? token-at tk)      (display "@"))
            ((eq? token-rbracks tk) (display ">>"))
            ((string=? "<<" tk)     (display "@<<"))
            ((string=? ">>" tk)     (display "@>>"))
            ((string=? "@" tk)      (display "@@"))
            ((string=? "%def " tk)  (display "@%def "))
            (else                   (display tk)))
      (dump-detokenized (input-cdr tks)))))

;;; El siguiente procedimiento imprime de manera "*bonita*" un árbol de razones de
;;; fallo.
(define (dump-error-tree err)
  (define indent-factor 2)
  (define (display-error-lvl err lvl)
    (define spaces (make-string (* lvl indent-factor) #\space))
    (cond ((list? err)
           (for-each (lambda (x)
                       (display-error-lvl x (+ lvl 1)))
                     err))
          ((pair? err)
           (display spaces)
           (display (car err))
           (newline)
           (display-error-lvl (cdr err) (+ lvl 1)))
          ((string? err)
           (display spaces)
           (display err)
           (newline))
          ((procedure? err)
           (display spaces)
           (let ((op (open-output-string)))
             (pp (unsyntax err) op)
             (let ((ip (open-input-string (get-output-string op))))
               (let ((lines (let recur ((line (read-line ip)))
                              (if (eof-object? line)
                                  '()
                                  (cons line (recur (read-line ip)))))))
                 (for-each (lambda (x)
                             (display x)
                             (newline)
                             (display spaces))
                           (drop-right lines 1))
                 (unless (null? lines)
                   (display (last lines))
                   (newline)))
               (close-input-port ip))
             (close-output-port op)))
          (else
           (display spaces)
           (display err)
           (newline))))
  (for-each (lambda (x)
              (display-error-lvl x 0))
            err))

(define-parser <program>
  (:+: (:alt: <docs> <code>)))

(define-parser <code>
  (wrap-code (:seq: (wrap-code-lbracks (:eq: token-lbracks))
                    (wrap-code-name    (:*: (:pred: string?)))
                    (wrap-code-rbreqs  (:eq: token-rbreqs))
                    (wrap-code-spaces  (:*: (:pred: string-spaces?)))
                    (wrap-code-newline (:eq: token-newline))
                    (wrap-code-lines   <code-lines>))))

(define wrap-code
  (:><: (lambda (match)
          ;; (list (cons 'code match))
          (assert (string? (car match)))
          (assert (list?   (cadr match)))
          (assert (null?   (cddr match)))
          (let ((defs (and-let* ((maybe-defs (last (cadr match)))
                                 ((pair? maybe-defs))
                                 ((eq? 'defs (car maybe-defs)))
                                 ((not (null? (cdr maybe-defs)))))
                        (cdr maybe-defs))))
            (list (make-code (list-ref match 0)
                             defs
                             (if defs
                                 (drop-right (cadr match) 1)
                                 (cadr match))))))
        (lambda (why)
          (list (cons "malformed code chunk"
                      why)))))

(define wrap-code-lbracks
  (:><: (constant '())
        (lambda (why)
          (list (cons "missing << in a starting code line"
                      why)))))

(define wrap-code-name
  (:><: (lambda (match)
          (list (apply string-append match)))
        (lambda (why)
          (list (cons "missing code name in a starting code line"
                      why)))))

(define wrap-code-rbreqs
  (:><: (constant '())
        (lambda (why)
          (list (cons "missing >>= in a starting code line"
                      why)))))

(define wrap-code-spaces
  (:><: (constant '()) id))

(define wrap-code-newline
  (:><: (constant '())
        (lambda (why)
          (list (cons "missing a new line at the end of a starting code line"
                      why)))))

(define wrap-code-lines
  (:><: (lambda (match) (list match)) id))

(define (string-spaces? x)
  (and (string? x)
       (not (string=? "" x))
       (string=? "" (string-trim x))))

(define-parser <code-lines>
  (:alt: (:seq: (:*: (:alt: (:pred: string?) <refs>))
                (wrap-content-newline (:eq: token-newline))
                <code-lines>)
         (wrap-code-line-end
          (:seq: (wrap-code-line-at      (:eq: token-at))
                 (wrap-code-line-spaces  (:+: (:pred: string-spaces?)))
                 (wrap-code-line-def     (:eq: token-def))
                 (wrap-code-line-names   (:+: (:pred: string?)))
                 (wrap-code-line-newline (:eq: token-newline))))
         :succeed:))

(define wrap-code-line-end
  (:><: (lambda (match)
          (list (cons 'defs match)))
        (lambda (why)
          (cons "malformed code @ %def line"
                why))))

(define wrap-code-line-at
  (:><: (constant '()) id))

(define wrap-code-line-spaces
  (:><: (constant '()) id))

(define wrap-code-line-def
  (:><: (constant '()) id))

(define wrap-code-line-names
  (:><: (lambda (match)
          (let recur ((chs (string->list (apply string-append match)))
                      (sub '()))
            (cond ((null? chs)
                   (if (null? sub)
                       sub
                       (cons (list->string (reverse sub))
                             '())))
                  ((char=? #\space (car chs))
                   (if (null? sub)
                       (recur (cdr chs) sub)
                       (cons (list->string (reverse sub))
                             (recur (cdr chs) '()))))
                  (else
                   (recur (cdr chs)
                          (cons (car chs) sub))))))
        (lambda (why)
          (cons "missing definitions names in @ %def line"
                why))))

(define wrap-code-line-newline
  (:><: (constant '())
        (lambda (why)
          (list (cons "missing a new line at the end of a @ %def line"
                      why)))))

(define-parser <refs>
  (wrap-refs (:seq: (wrap-refs-lbrack (:eq: token-lbracks))
                    (wrap-refs-name   (:*: (:pred: string?)))
                    (wrap-refs-rbrack (:eq: token-rbracks)))))

(define wrap-refs
  (:><: (lambda (match)
          ;; (list (cons 'refs match))
          (assert (string? (car match)))
          (assert (null?   (cdr match)))
          (list (make-refs (car match))))
        (lambda (why)
          (list (cons "malformed refs"
                      why)))))

(define wrap-refs-lbrack
  (:><: (constant '()) id))

(define wrap-refs-name
  (:><: (lambda (match)
          (list (apply string-append match)))
        id))

(define wrap-refs-rbrack
  (:><: (constant '())
        (lambda (why)
          (list (cons "bad refs ending, there must be just text and then >>"
                      why)))))

(define-parser <docs>
  (wrap-docs (:alt: (:seq: (wrap-docs-at (:eq: token-at))
                           (wrap-docs-spaces (:*: (:pred: string-spaces?)))
                           (wrap-docs-newline (:eq: token-newline))
                           <docs-lines>)
                    <docs-lines>)))

(define wrap-docs
  (:><: (lambda (match)
          ;; (list (cons 'docs match))
          (assert (list? (car match)))
          (assert (null? (cdr match)))
          (list (make-docs (car match))))
        (lambda (why)
          (list (cons "malformed documentation chunk"
                      why)))))

(define wrap-docs-at
  (:><: (constant '()) id))

(define wrap-docs-spaces
  (:><: (constant '()) id))

(define wrap-docs-newline
  (:><: (constant '()) id))

(define-parser <docs-lines>
  (wrap-docs-lines (:+: (wrap-docs-line-content
                         (:seq: (:*: (:alt: (:pred: string?) <refs>))
                                (wrap-content-newline (:eq: token-newline)))))))

(define wrap-docs-lines
  (:><: (lambda (match)
          (list match))
        (lambda (why)
          (list (cons "there must be at least one valid documentation line"
                      why)))))

(define wrap-docs-line-content
  (:><: id
        (lambda (why)
          (list (cons "malformed documentation line"
                      why)))))

(define wrap-content-newline
  (:><: (constant '(#\newline))
        (lambda (why)
          (list (cons "missing a new line in chunk content"
                      why)))))

(define (make-docs content)
  (list ':docs content))

(define (docs? x)
  (and (list? x)
       (eq? ':docs (car x))
       (= 2 (length x))
       (list? (list-ref x 1))))

(define (docs-content docs)
  (assert (docs? docs))
  (list-ref docs 1))

(define (make-refs name)
  (list ':refs name))

(define (refs? x)
  (and (list? x)
       (eq? ':refs (car x))
       (= 2 (length x))
       (string? (list-ref x 1))))

(define (refs-name refs)
  (assert (refs? refs))
  (list-ref refs 1))
