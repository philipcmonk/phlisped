#lang racket

(require "disp.ss")

(require (for-syntax racket/system))
(define-syntax (require-dir syn)
 (let* ((dir (cadr (syntax->datum syn)))
        (phls (map (lambda (f) (string-append dir "/" f)) (filter (lambda (f) (regexp-match ".phl$" f)) (map path->string (directory-list dir))))))
  (for-each (lambda (phl) (system* "bin/phlisp" (string-append "-o " (regexp-replace ".phl$" phl ".rkt")) phl)) phls)
  (let* ((rkts (map (lambda (f) (string-append dir "/" f)) (filter (lambda (f) (regexp-match ".rkt$" f)) (map path->string (directory-list dir)))))
         (rkts2 (map (lambda (rkt) `(prefix-in ,(string->symbol (string-append "com-" rkt ":")) ,rkt)) rkts))
	 (regs (map (lambda (rkt) (string->symbol (string-append "com-" rkt ":data"))) rkts)))
   (datum->syntax syn `(begin
			(require ,@rkts2)
			(add-key-evs (append ,@regs)))))))

(require-dir "commands")

