#lang racket

(require sgl sgl/gl)
(require "common.ss")
(require "def-painter.ss")

(provide make-linear-vertical-v11n other-v11n-utterance-total-height other-v11n-utterance-runtime-vals)

(struct other-v11n-utterance utterance (total-height runtime-vals))

(define (make-linear-vertical-v11n #:rectangle-drawer (rectangle-drawer values) #:text-generator (text-generator (lambda (text) text)))
 (v11n
  (def-painter
   #:drawer 
    (lambda (text x y w h text-w text-h clr u tree center)
     (rectangle-drawer clr x y w h tree)
     (if (and (< (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
              (< (/ text-h (whole-tree-zoom tree)) h))
      (draw-text
       (text-generator text)
       (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
       (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
       (car clr)
       tree)
      '())
     (map
      (lambda (val n)
       (draw-text
        (format "~a" val)
        (+ x w 100 (* 40 n))
       (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
        (car clr)
        tree))
      (other-v11n-utterance-runtime-vals u)
      (build-list (length (other-v11n-utterance-runtime-vals u)) identity))))

  (lambda (n x y w row siblings tree)
   (let node->utterance ((n n) (x x) (y y) (row row) (siblings siblings) (tree tree))
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
      x
      y
      (box-width ((node-text-func n) n))
      (node-height n tree)
      (box-width ((node-text-func n) n))
      (box-height ((node-text-func n) n))
      children
      (get-color n tree)
      (foldl + (node-height n tree) (map other-v11n-utterance-total-height children))
      (map cadr (list-ref (node-data n) 7))))))

  (lambda (root x y tree)
   (let find-utterance ((root root) (x x) (y y))
    (with
     ((if (or
           (< y (+ (utterance-y root) (utterance-h root)))
           (> y (+ (utterance-y root) (other-v11n-utterance-total-height root))))
       root
       (ormap
        (lambda (child)
         (if (< y (+ (utterance-y child) (other-v11n-utterance-total-height child)))
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
