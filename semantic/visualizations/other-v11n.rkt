#lang racket

(require sgl sgl/gl)
(require "../common.ss")
(require "helpers/linear-vertical-v11n.ss")

(provide visualization)

(define visualization
 (make-linear-vertical-v11n
  #:rectangle-drawer
   (lambda (clr x y w h tree)
    (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h))))

