#lang racket

(require "compiler.rkt")

(define run-code #f)
(define out-file #f)

(define filename
 (command-line
  #:program "phlisp"
  #:once-each
  (("-o" "--output") output-file "output compiled code to file (default is stdout)"
   (set! out-file output-file))
  (("-r" "--run") "run code"
   (set! run-code #t))
  #:args
  (filename)
  filename))

(define G (call-with-input-file filename (lambda (f) (read f))))
(define compiled (reify G 0 #f))
(if out-file
 (call-with-output-file out-file #:exists 'truncate (lambda (f) (display "#lang racket\n\n" f) (map (curryr displayln f) compiled)))
 (map display compiled))
(when run-code
 (let ((ns (make-base-namespace)))
  (eval compiled ns)))

