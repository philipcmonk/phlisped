#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with))
(require "../core/gnode.rkt")
(require "../core/compiler.rkt")

(provide data)

(define (reify-code event) (display (reify G 0 #f)) (newline))

(define data
 (list #\r reify-code))

