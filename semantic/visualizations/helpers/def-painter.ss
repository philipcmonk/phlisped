#lang racket

(require sgl sgl/gl)
(require "../../common.ss")

(provide def-painter)

(define (inv? x y w h tree)
 (or
  (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
  (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
  (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
  (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))

(define (center offset lenwhole lenpiece start width)
 (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
  (if (< visible-width lenpiece)
   (if (< offset start)
    (- (+ offset lenwhole) lenpiece)
    offset)
   (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))

(define (def-painter #:drawer (drawer values) #:invisible? (invisible? inv?))
 (lambda (tree)
  (with
   ((apply gl-scissor (whole-tree-dim tree))
    (apply gl-viewport (whole-tree-dim tree))
    (gl-matrix-mode 'projection)
    (gl-load-identity)
    (gl-ortho
     (- (whole-tree-offset-x tree))
     (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
     (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
     (whole-tree-offset-y tree)
     -1.0
     1.0)
    (gl-matrix-mode 'modelview)
    (gl-load-identity)
    (utterance-paint (whole-tree-utterance-tree tree) tree))

   (utterance-paint (u tree)
    (let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
           (x (utterance-x u))
           (y (utterance-y u))
           (w (utterance-w u))
           (h (utterance-h u))
           (text-w (utterance-text-w u))
           (text-h (utterance-text-h u))
           (args (utterance-args u))
           (clr (utterance-clr u)))
     (if (invisible? x y w h tree)
      '()
      (begin
       (drawer text x y w h text-w text-h clr u tree center)
;       (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
;       (if (< (whole-tree-zoom tree) 1) '()
;        (draw-text
;         text
;         (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
;         (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
;         (car clr)
;         tree))
       (for-each touch (map (lambda (arg) (future (lambda () (utterance-paint arg tree)))) args)))))))))


