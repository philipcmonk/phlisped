#lang racket

(require sgl sgl/gl)
(require "../core/common.rkt")
(require "helpers/default-v11n.rkt")

(provide visualization)

(define visualization
 (make-default-v11n
  #:child-w-generator (lambda (n) (foldl max 0 (map (lambda (arg) (box-width ((node-text-func arg) arg))) (node-args n))))
  #:child-x-generator (lambda (data w n) (+ (car data) (if (zero? w) (box-width ((node-text-func n) n)) w)))
  #:child-y-generator (lambda (data arg tree) (cadr data))
  #:update-x-generator (lambda (data res) (car data))
  #:update-y-generator (lambda (data res arg tree) (+ (cadr data) 
                      (let height ((n arg))
                       (if (closed? n tree)
                        (box-height ((node-text-func n) n))
                        (max
                         (box-height ((node-text-func n) n))
                         (foldl + 0 (map height (node-args n))))))
                                                      ))
  #:width-generator (lambda (w n children) (if (zero? w) (box-width ((node-text-func n) n)) w))
  #:height-generator (lambda (n tree)
                      (let height ((n n))
                       (if (closed? n tree)
                        (box-height ((node-text-func n) n))
                        (max
                         (box-height ((node-text-func n) n))
                         (foldl + 0 (map height (node-args n)))))))))

;(define (node-maj-dim n tree)
; (if (closed? n tree)
;  (box-maj-dim ((node-text-func n) (node-data n)))
;  (max
;   (box-maj-dim ((node-text-func n) (node-data n)))
;   (foldl
;    +
;    0
;    (map (lambda (arg) (node-maj-dim arg tree)) (node-args n))))))

;(define default-vertical-v11n
; (v11n
;  (def-painter
;   #:drawer
;    (lambda (text x y w h text-w text-h clr u tree center)
;     (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
;     (if (and (< (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
;              (< (/ text-h (whole-tree-zoom tree)) h))
;      (draw-text
;       text
;       (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
;       (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
;       (car clr)
;       tree)
;      '())))
;
;  (lambda (n x y w row siblings tree)
;   (let node->utterance ((n n) (x x) (y y) (w w) (row row) (siblings siblings) (tree tree))
;    (with
;     ((let ((children 
;             (if (closed? n tree)
;              '()
;              (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (node-width arg tree)) (node-args n))) -1)))
;               (caddr
;                (foldl
;                 (lambda (arg data)
;                  (let ((res (node->utterance
;                              arg
;                              (if VERTICAL (+ (car data) w) (car data))
;                              (if VERTICAL (cadr data) (+ (cadr data) (node-height arg tree)))
;                              child-w
;                              (+ 1 row)
;                              (- (length (node-args n)) 1)
;                              tree)))
;                   (list
;                    (if VERTICAL (car data) (+ (car data) (utterance-w res)))
;                    (if VERTICAL (+ (cadr data) (node-height arg tree)) (cadr data))
;                    (if (null? res)
;                     (caddr data)
;                     (append
;                      (caddr data)
;                      (list res))))))
;                 (list x y '())
;                 (node-args n)))))))
;       (utterance
;        n
;        x
;        y
;        (if VERTICAL w (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children))))
;        (node-height n tree)
;        (box-width ((node-text-func n) n))
;        (box-height ((node-text-func n) n))
;        children
;        (get-color n tree)))))))
;
;  (lambda (root x y tree)
;   (let find-utterance ((root root) (x x) (y y) (tree tree))
;    (with
;     ((if (or
;           (above-bottom-of-utterance?)
;           (utterance-is-closed?)
;           (has-no-children?)
;           (is-to-the-right-of-utterance?))
;       root
;       (pass-on-to-child)))
;   
;     (above-bottom-of-utterance? ()
;      (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root))))
;   
;     (utterance-is-closed? ()
;      (closed? (utterance-node root) tree))
;           
;     (has-no-children? ()
;      (null? (node-args (utterance-node root))))
;   
;     (is-to-the-right-of-utterance? ()
;      (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
;   
;     (pass-on-to-child ()
;      (ormap
;       (lambda (child)
;        (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
;         (find-utterance child x y tree)
;         #f))
;       (utterance-args root))))))
;
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

