#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with))
(require "../graph.ss")
(require "../compiler.rkt")

(provide data)

(define (reify-code event) (display (reify G 0 #f)) (newline))

(define data
 (list #\r reify-code))

