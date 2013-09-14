#lang racket

(require sgl sgl/gl)
(require "common.ss")

(provide default-v11n)

(define default-v11n (v11n
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
                          -100.0
                          100.0)
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
                       (let node->utterance ((n n) (x x) (y y) (w w) (row row) (siblings siblings) (tree tree))
                        (with
                         ((let ((children 
                                 (if (closed? n tree)
                                  '()
                                  (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (node-width arg tree)) (node-args n))) -1)))
                                   (caddr
                                    (foldl
                                     (lambda (arg data)
                                      (let ((res (node->utterance
                                                  arg
                                                  (if VERTICAL (+ (car data) w) (car data))
                                                  (if VERTICAL (cadr data) (+ (cadr data) (node-height arg tree)))
                                                  child-w
                                                  (+ 1 row)
                                                  (- (length (node-args n)) 1)
                                                  tree)))
                                       (list
                                        (if VERTICAL (car data) (+ (car data) (utterance-w res)))
                                        (if VERTICAL (+ (cadr data) (node-height arg tree)) (cadr data))
                                        (if (null? res)
                                         (caddr data)
                                         (append
                                          (caddr data)
                                          (list res))))))
                                     (list x y '())
                                     (node-args n)))))))
                           (utterance
                            n
                            x
                            y
                            (if VERTICAL w (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children))))
                            (node-height n tree)
                            (box-width ((node-text-func n) n))
                            (box-height ((node-text-func n) n))
                            children
                            (get-color n tree)))))))

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
                          (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root))))
                       
                         (utterance-is-closed? ()
                          (closed? (utterance-node root) tree))
                               
                         (has-no-children? ()
                          (null? (node-args (utterance-node root))))
                       
                         (is-to-the-right-of-utterance? ()
                          (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
                       
                         (pass-on-to-child ()
                          (ormap
                           (lambda (child)
                            (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
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
