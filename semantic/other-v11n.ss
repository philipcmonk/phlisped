#lang racket

(require sgl sgl/gl)
(require "common.ss")
(require "linear-vertical-v11n.ss")

(provide other-v11n)

(define other-v11n
 (make-linear-vertical-v11n
  #:rectangle-drawer
   (lambda (clr x y w h tree)
    (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h))))

