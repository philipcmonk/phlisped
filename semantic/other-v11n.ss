#lang racket

(require sgl sgl/gl)
(require "common.ss")

(provide other-v11n)

(struct other-v11n-utterance utterance (total-height runtime-vals))

(define other-v11n (v11n
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
                         (with
                          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
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
                              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
                              (if (< (whole-tree-zoom tree) 1) '()
                               (draw-text
                                text
                                (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                (car clr)
                                tree))
                              (map
                               (lambda (val n)
                                (draw-text
                                 (format "~a" val)
                                 (+ x w 100 (* 40 n))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                 (car clr)
                                 tree))
                               (other-v11n-utterance-runtime-vals u)
                               (build-list (length (other-v11n-utterance-runtime-vals u)) identity))
                              (for-each (lambda (arg) (utterance-paint arg tree)) args)))))
                  
                         (invisible? (x y w h tree)
                          (or
                           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
                           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
                           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
                    
                         (center (offset lenwhole lenpiece start width)
                          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
                           (if (< visible-width lenpiece)
                            (if (< offset start)
                             (- (+ offset lenwhole) lenpiece)
                             offset)
                            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))

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
;                          (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children)))
;                          (foldl + (node-height n tree) (map utterance-h children))
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
