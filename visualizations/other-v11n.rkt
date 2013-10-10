#lang racket

(require sgl sgl/gl)
(require "../core/common.rkt")
(require "helpers/linear-vertical-v11n.rkt")

(provide visualization)

(define visualization
 (make-linear-vertical-v11n
  #:rectangle-drawer
   (lambda (u tree)
    (draw-rectangle (if (eq? Selected-tree tree) (cdr (utterance-clr u)) (map (curryr / 3) (cdr (utterance-clr u)))) (cartesian-utterance-x u) (cartesian-utterance-y u) (cartesian-utterance-w u) (cartesian-utterance-h u)))))

