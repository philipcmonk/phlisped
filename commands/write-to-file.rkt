#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with))
(require "../core/gnode.rkt")

(provide data)

(define (write-g-to-file event)
 (graph->file G))

(define data
 (list 'f2 write-g-to-file))

