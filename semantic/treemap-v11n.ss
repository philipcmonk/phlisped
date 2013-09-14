#lang racket

(require sgl sgl/gl)
(require "common.ss")
(require "def-painter.ss")

(provide treemap-v11n)

(struct treemap-utterance utterance (total-width total-height))

(define treemap-v11n
 (v11n
  (def-painter
   #:drawer 
    (lambda (text x y w h text-w text-h clr u tree center)
     (let ((t-w (treemap-utterance-total-width u))
           (t-h (treemap-utterance-total-height u)))
      (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y t-w t-h)
      (if (and (< (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
               (< (/ text-h (whole-tree-zoom tree)) h))
       (draw-text
        text
        (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
        (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
        (car clr)
        tree)
       '())))

    #:invisible? (lambda l #f))

  (lambda (n x y w row siblings tree)
   (let node->utterance ((n n) (x x) (y y) (w (whole-tree-w tree)) (h (whole-tree-h tree)) (dir 'horizontal) (row row) (siblings siblings) (tree tree))
    (let ((children
           (if (or (closed? n tree) (null? (node-args n)))
            '()
            (let* ((num-args (+ 1 (length (node-args n))))
                   (child-wh (/ (if (eq? dir 'horizontal) w h) num-args)))
             (map
              (lambda (arg n)
               (if (eq? dir 'horizontal)
                (node->utterance
                 arg
                 (+ x (* n child-wh))
                 y
                 child-wh
                 h
                 'vertical
                 (+ 1 row)
                 (- num-args 1)
                 tree)
                (node->utterance
                 arg
                 x
                 (+ y (* n child-wh))
                 w
                 child-wh
                 'horizontal
                 (+ 1 row)
                 (- num-args 1)
                 tree)))
              (node-args n)
              (map (curry + 1) (build-list (length (node-args n)) identity)))))))
     (treemap-utterance
      n
      x
      y
      (or (and (or (eq? dir 'vertical) (closed? n tree) (null? (node-args n))) w) (/ w (+ 1 (length (node-args n)))))
      (or (and (or (eq? dir 'horizontal) (closed? n tree) (null? (node-args n))) h) (/ h (+ 1 (length (node-args n)))))
      (box-width ((node-text-func n) n))
      (box-height ((node-text-func n) n))
      children
      (get-color n tree)
      w
      h))))

  (lambda (root x y tree)
   (or
    (let find-utterance ((root root))
     (if (or (and (= (utterance-w root) (treemap-utterance-total-width root)) (< y (+ (utterance-y root) (utterance-h root))))
             (and (= (utterance-h root) (treemap-utterance-total-height root)) (< x (+ (utterance-x root) (utterance-w root))))
             (null? (utterance-args root)))
      root
      (ormap
       (lambda (child)
        (if (and
             (>= y (utterance-y child))
             (< y (+ (utterance-y child) (treemap-utterance-total-height child)))
             (>= x (utterance-x child))
             (< x (+ (utterance-x child) (treemap-utterance-total-width child))))
         (find-utterance child)
         #f))
       (utterance-args root))))
    root))

  (lambda (dir event)
   (cond
    ((eq? dir 'up)
     (set-whole-tree-zoom! Selected-tree (* (/ 1.2) (whole-tree-zoom Selected-tree))))
    ((eq? dir 'down)
     (set-whole-tree-zoom! Selected-tree (* 1.2 (whole-tree-zoom Selected-tree))))
    ((eq? dir 'left)
     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
    ((eq? dir 'right)
     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

