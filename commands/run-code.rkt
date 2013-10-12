#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")
(require "../core/compiler.rkt")

(provide data)

(define (run-code event)
 (let ((code (reify G 0 #t))
       (ns (make-base-namespace)))
  (print code) (newline)
  (eval '(require racket "graph.ss") ns)
  (eval '(define Next-r -1) ns)
  (eval '(define stack '()) ns)
  (eval '(define h (hash)) ns)
  (eval code ns)
  (set-runtime-vals (eval 'h ns))
  (update-data)
  (update-childfuncs child-fun)))

(define data
 (list '(#\G run) run-code))

