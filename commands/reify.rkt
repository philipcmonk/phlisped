#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")
(require "../core/compiler.rkt")

(provide data)

(define (reify-code event) (display (reify G 0 #f)) (newline))

(define data
 (list '(#\r reify) reify-code))

