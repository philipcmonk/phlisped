#lang racket

(require sgl sgl/gl)
(require "../../core/common.rkt")
(require "def-painter.rkt")
(require "stdlib.rkt")

(provide make-default-v11n cartesian-utterance-x cartesian-utterance-y cartesian-utterance-w cartesian-utterance-h cartesian-utterance-text-w cartesian-utterance-text-h)

(define (make-default-v11n #:child-w-generator (child-w-generator values) #:child-x-generator (child-x-generator values) #:child-y-generator (child-y-generator values) #:update-x-generator (update-x-generator values) #:update-y-generator (update-y-generator values) #:width-generator (width-generator values) #:height-generator (height-generator values))
 (v11n
  (def-painter
   #:drawer generic-drawer)

  cartesian-utterance-generator

  (lambda (root x y tree)
   (let find-utterance ((root root) (x x) (y y) (tree tree))
    (with
     ((if (or
           (above-bottom-of-utterance?)
           (utterance-is-closed?)
           (has-no-children?)
           (is-to-the-right-of-utterance?))
       root
       (pass-on-to-child)))

     (above-bottom-of-utterance? ()
      (< (min-dim x y) (+ (cartesian-utterance-min-dim root) (cartesian-utterance-min-dim-span root))))

     (utterance-is-closed? ()
      (closed? (utterance-node root) tree))

     (has-no-children? ()
      (null? (node-args (utterance-node root))))

     (is-to-the-right-of-utterance? ()
      (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (cartesian-utterance-maj-dim baby) (cartesian-utterance-maj-dim-span baby)))))

     (pass-on-to-child ()
      (ormap
       (lambda (child)
        (if (< (maj-dim x y) (+ (cartesian-utterance-maj-dim child) (cartesian-utterance-maj-dim-span child)))
         (find-utterance child x y tree)
         #f))
       (utterance-args root))))))

  horiz-scroller))
;  (lambda (dir event)
;   (cond
;    ((eq? dir 'up)
;     (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
;    ((eq? dir 'down)
;     (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
;    ((eq? dir 'left)
;     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
;    ((eq? dir 'right)
;     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

(define (cartesian-utterance-maj-dim u) (if VERTICAL (cartesian-utterance-y u) (cartesian-utterance-x u)))
(define (cartesian-utterance-maj-dim-span u) (if VERTICAL (cartesian-utterance-h u) (cartesian-utterance-w u)))
(define (cartesian-utterance-min-dim u) (if VERTICAL (cartesian-utterance-x u) (cartesian-utterance-y u)))
(define (cartesian-utterance-min-dim-span u) (if VERTICAL (cartesian-utterance-w u) (cartesian-utterance-h u)))

