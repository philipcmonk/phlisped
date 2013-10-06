#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with))
(require "../graph.ss")

(provide data)

(define (write-g-to-file event)
 (graph->file G))

(define data
 (list 'f2 write-g-to-file))

