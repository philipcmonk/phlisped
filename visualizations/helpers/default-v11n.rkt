#lang racket

(require sgl sgl/gl)
(require "../../core/common.rkt")
(require "def-painter.rkt")
(require "stdlib.rkt")

(provide make-default-v11n cartesian-utterance-x cartesian-utterance-y cartesian-utterance-w cartesian-utterance-h cartesian-utterance-text-w cartesian-utterance-text-h)

;(struct cartesian-utterance utterance (x y w h text-w text-h))

(define (make-default-v11n #:child-w-generator (child-w-generator values) #:child-x-generator (child-x-generator values) #:child-y-generator (child-y-generator values) #:update-x-generator (update-x-generator values) #:update-y-generator (update-y-generator values) #:width-generator (width-generator values) #:height-generator (height-generator values))
 (v11n
  (def-painter
   #:drawer
    (lambda (u tree)
     (generic-drawer u tree)))

  (lambda (n tree)
   (let node->utterance ((n n) (x 0) (y 0) (w 0) (row 0) (siblings '()) (tree tree))
    (with
     ((let ((children 
             (if (closed? n tree)
              '()
              (let ((child-w (child-w-generator n)))
               (caddr
                (foldl
                 (lambda (arg data)
                  (let ((res (node->utterance
                              arg
                              (child-x-generator data w n)
                              (child-y-generator data arg tree)
                              child-w
                              (+ 1 row)
                              (- (length (node-args n)) 1)
                              tree)))
                   (list
                    (update-x-generator data res)
                    (update-y-generator data res arg tree)
                    (if (null? res)
                     (caddr data)
                     (append
                      (caddr data)
                      (list res))))))
                 (list x y '())
                 (node-args n)))))))
       (cartesian-utterance
        n
        children
        (get-color n tree)
        x
        y
        (width-generator w n children)
        (height-generator n tree)
        (box-width ((node-text-func n) n))
        (box-height ((node-text-func n) n))))))))

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

  (lambda (dir event)
   (cond
    ((eq? dir 'up)
     (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
    ((eq? dir 'down)
     (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
    ((eq? dir 'left)
     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
    ((eq? dir 'right)
     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

(define (cartesian-utterance-maj-dim u) (if VERTICAL (cartesian-utterance-y u) (cartesian-utterance-x u)))
(define (cartesian-utterance-maj-dim-span u) (if VERTICAL (cartesian-utterance-h u) (cartesian-utterance-w u)))
(define (cartesian-utterance-min-dim u) (if VERTICAL (cartesian-utterance-x u) (cartesian-utterance-y u)))
(define (cartesian-utterance-min-dim-span u) (if VERTICAL (cartesian-utterance-w u) (cartesian-utterance-h u)))

