#|

# (`doc/gendoc.scm`)

Simón

|#

(define (gendoc ifilenames ofilename)
  (with-output-to-file ofilename
    (lambda ()
      (for-each (lambda (ifilename)
                  (with-input-from-file ifilename
                    display-reversed-markdown))
                ifilenames))))
#||#

(define (display-reversed-markdown)
  (let loop ((line     (read-line))
             (in-code? #f))
    (unless (eof-object? line)
      (cond ((string=? "#|" line)
             (let write-markdown ((line (read-line)))
               (if (or (eof-object? line)
                       (string=? "|#" line))
                   (loop (read-line) #f)
                   (begin
                     (display line)
                     (newline)
                     (write-markdown (read-line))))))
            ((string=? "#||#" line)
             (when in-code?
               (display "```\n"))
             (loop (read-line) #f))
            ((string=? "" (string-trim line))
             (when in-code?
               (newline))
             (loop (read-line) in-code?))
            (else
             (unless in-code?
               (display "```scheme\n"))
             (display line)
             (newline)
             (loop (read-line) #t))))))
#||#

(define *doc-filenames*
  (list "preamble.scm"
        "../tangle.scm"
        "../lib/utils.scm"
        "../lib/input.scm"
        "../lib/parcomb.scm"
        "gendoc.scm"))

(define *doc-output* "README.md")

(gendoc *doc-filenames* *doc-output*)
(run-shell-command
 (string-append
  "pandoc"
  " -V lang=es"
  " -V papersize=letter"
  " -V geometry=margin=1in"
  " -V fontsize=11pt"
  " -V mainfont=\"Linux Libertine O\""
  " -V sansfont=\"Linux Biolinum O\""
  " -V monofont=\"Inconsolata LGC\""
  " " *doc-output*
  " --number-sections"
  " --latex-engine=xelatex"
  " -o " *doc-output* ".pdf"))
#||#
