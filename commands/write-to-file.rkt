#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")

(provide data)

(define (write-g-to-file event)
 (graph->file G))

(define data
 (list '(f2 write w) write-g-to-file))

