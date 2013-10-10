#lang racket

(require sgl sgl/gl)
(require "../../core/common.rkt")
(require "def-painter.ss")
(require "stdlib.rkt")

(provide make-linear-vertical-v11n other-v11n-utterance-total-height other-v11n-utterance-runtime-vals)

;(struct cartesian-utterance utterance (x y w h text-w text-h))

(struct other-v11n-utterance cartesian-utterance (total-height runtime-vals))

(define (make-linear-vertical-v11n #:rectangle-drawer (drawer draw-rectangle-u) #:text-generator (text-generator straight-text))
 (v11n
  (def-painter
   #:drawer 
    (lambda (u tree)
     (generic-drawer u tree #:drawer drawer #:text text-generator)))

  (lambda (n tree)
   (let node->utterance ((n n) (x 0) (y 0) (row 0) (siblings '()) (tree tree))
    (let ((children 
           (if (closed? n tree)
            '()
            (cadr
             (foldl
              (lambda (arg data)
               (let ((res (node->utterance
                           arg
                           (+ x 10)
                           (car data)
                           (+ 1 row)
                           (- (length (node-args n)) 1)
                           tree)))
                (if (null? res)
                 (list
                  0
                  (cadr data))
                 (list
                  (+ (car data) (other-v11n-utterance-total-height res))
                  (append
                   (cadr data)
                   (list res))))))
              (list (+ y (node-height n tree)) '())
              (node-args n))))))
     (other-v11n-utterance
      n
      children
      (get-color n tree)
      x
      y
      (box-width ((node-text-func n) n))
      (node-height n tree)
      (box-width ((node-text-func n) n))
      (box-height ((node-text-func n) n))
      (foldl + (node-height n tree) (map other-v11n-utterance-total-height children))
      (map cadr (list-ref (node-data n) 7))))))

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
