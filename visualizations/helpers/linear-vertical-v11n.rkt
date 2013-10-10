#lang racket

(require sgl sgl/gl)
(require "../../core/common.rkt")
(require "def-painter.ss")
(require "stdlib.rkt")

(provide make-linear-vertical-v11n other-v11n-utterance-total-height)

(define (make-linear-vertical-v11n #:rectangle-drawer (drawer draw-rectangle-u) #:text-generator (text-generator straight-text))
 (v11n
  (def-painter
   #:drawer 
    (lambda (u tree)
     (generic-drawer u tree #:drawer drawer #:text text-generator)))

  linear-vertical-utterance-generator

  (lambda (root x y tree)
   (let find-utterance ((root root) (x x) (y y))
    (with
     ((if (or
           (< y (+ (cartesian-utterance-y root) (cartesian-utterance-h root)))
           (> y (+ (cartesian-utterance-y root) (other-v11n-utterance-total-height root))))
       root
       (ormap
        (lambda (child)
         (if (< y (+ (cartesian-utterance-y child) (other-v11n-utterance-total-height child)))
          (find-utterance child x y)
          #f))
        (utterance-args root)))))))

  (lambda (dir event)
   (cond
    ((eq? dir 'up)
     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
    ((eq? dir 'down)
     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))
    ((eq? dir 'left)
     (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
    ((eq? dir 'right)
     (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))))))
