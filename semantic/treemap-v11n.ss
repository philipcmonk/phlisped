#lang racket

(require sgl sgl/gl)
(require "common.ss")
(require "def-painter.ss")

(provide treemap-v11n)

(struct treemap-utterance utterance (total-width total-height))

(define treemap-v11n (letrec ((v147 (lambda (a391 a392 a393 a396 a397 a398 a399) (letrec ((v149 (lambda (a182 a183 a184 a210 a247 a249 a336 a390) (letrec ((v246 a247)(v335 a336)(v179 a182) (v209 a210) (v180 a183) (v181 a184) (v201 a390) (v248 a249) (v262 ((node-text-func v179) v179)) (v265 (if (or (closed? v179 v201) (null? (node-args v179))) null (letrec ((v287 (lambda (a316 a339) (letrec ((v343 (+ 1 (length (node-args v179))))(v324 (/ (if (eq? v248 (quote horizontal)) v209 v246) v343)) (v315 a316) (v338 a339) ) (if (eq? v248 (quote horizontal)) (v149 v315 (begin (displayln v180) (displayln v338) (displayln v324) (displayln (+ v180 (* v338) v324)) (displayln "t") (+ v180 (* v338 v324))) v181 v324 v246 (quote vertical) (+ 1 v335) v201) (v149 v315 v180 (+ v181 (* v338 v324)) v209 v324 (quote horizontal) (+ 1 v335) v201)))))) (map v287 (node-args v179) (map (curry + 1) (build-list (length (node-args v179)) identity))))))  ) (treemap-utterance v179 v180 v181 (or (and (or (eq? v248 (quote vertical)) (closed? v179 v201) (null? (node-args v179))) v209) (/ v209 (+ 1 (length (node-args v179))))) (or (and (or (eq? v248 (quote horizontal)) (closed? v179 v201) (null? (node-args v179))) v246) (/ v246 (+ 1 (length (node-args v179))))) (box-width v262) (box-height v262) v265 (get-color v179 v201) v209 v246)))) (v151 a391) (v153 a392) (v155 a393) (v157 -) (v164 a397) (v166 -) (v168 -) (v171 a399)) (v149 v151 v153 v155 (whole-tree-w v171) (whole-tree-h v171) (quote horizontal) v164 v171))))) (v11n (letrec ((v9 (lambda (a134 a135 a136 a137 a138 a139 a140 a141 a143 a144 a145) (letrec ((v19 a144) (v29 a141) (v37 a135) (v38 a136) (v58 a137) (v69 a138)) (begin (letrec ((v39 (treemap-utterance-total-width a143)) (v40 (treemap-utterance-total-height a143))) (draw-rectangle (if (eq? Selected-tree v19) (cdr v29) (map (curry / 3) (cdr v29))) v37 v38 v39 v40)) (letrec ((v52 a139) (v64 a140)) (if (and (< (/ (- v52 PADDING) (whole-tree-zoom v19)) v58) (< (/ v64 (whole-tree-zoom v19)) v69)) (letrec ((v73 a134) (v76 a145)) (draw-text v73 (v76 v37 v58 (- v52 PADDING) (- (whole-tree-offset-x v19)) (whole-tree-w v19)) (+ v64 -3 (v76 v38 v69 v64 (- (whole-tree-offset-y v19)) (whole-tree-h v19))) (car v29) v19)) null)))))) (v123 (lambda (a129 a130 a131 a132 a133) #f))) (def-painter #:drawer v9 #:invisible? v123)) v147
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
     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree)))))))))

;(define treemap-v11n
; (v11n
;  (def-painter
;   #:drawer 
;    (lambda (text x y w h text-w text-h clr u tree center)
;     (let ((t-w (treemap-utterance-total-width u))
;           (t-h (treemap-utterance-total-height u)))
;      (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y t-w t-h)
;      (if (and (< (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
;               (< (/ text-h (whole-tree-zoom tree)) h))
;       (draw-text
;        text
;        (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
;        (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
;        (car clr)
;        tree)
;       '())))
;
;    #:invisible? (lambda l #f))
;
;  (lambda (n x y w row siblings tree)
;   (let node->utterance ((n n) (x x) (y y) (w (whole-tree-w tree)) (h (whole-tree-h tree)) (dir 'horizontal) (row row) (siblings siblings) (tree tree))
;    (let ((children
;           (if (or (closed? n tree) (null? (node-args n)))
;            '()
;            (let* ((num-args (+ 1 (length (node-args n))))
;                   (child-wh (/ (if (eq? dir 'horizontal) w h) num-args)))
;             (map
;              (lambda (arg n)
;               (if (eq? dir 'horizontal)
;                (node->utterance
;                 arg
;                 (+ x (* n child-wh))
;                 y
;                 child-wh
;                 h
;                 'vertical
;                 (+ 1 row)
;                 (- num-args 1)
;                 tree)
;                (node->utterance
;                 arg
;                 x
;                 (+ y (* n child-wh))
;                 w
;                 child-wh
;                 'horizontal
;                 (+ 1 row)
;                 (- num-args 1)
;                 tree)))
;              (node-args n)
;              (map (curry + 1) (build-list (length (node-args n)) identity)))))))
;     (treemap-utterance
;      n
;      x
;      y
;      (or (and (or (eq? dir 'vertical) (closed? n tree) (null? (node-args n))) w) (/ w (+ 1 (length (node-args n)))))
;      (or (and (or (eq? dir 'horizontal) (closed? n tree) (null? (node-args n))) h) (/ h (+ 1 (length (node-args n)))))
;      (box-width ((node-text-func n) n))
;      (box-height ((node-text-func n) n))
;      children
;      (get-color n tree)
;      w
;      h))))
;
;  (lambda (root x y tree)
;   (or
;    (let find-utterance ((root root))
;     (if (or (and (= (utterance-w root) (treemap-utterance-total-width root)) (< y (+ (utterance-y root) (utterance-h root))))
;             (and (= (utterance-h root) (treemap-utterance-total-height root)) (< x (+ (utterance-x root) (utterance-w root))))
;             (null? (utterance-args root)))
;      root
;      (ormap
;       (lambda (child)
;        (if (and
;             (>= y (utterance-y child))
;             (< y (+ (utterance-y child) (treemap-utterance-total-height child)))
;             (>= x (utterance-x child))
;             (< x (+ (utterance-x child) (treemap-utterance-total-width child))))
;         (find-utterance child)
;         #f))
;       (utterance-args root))))
;    root))
;
;  (lambda (dir event)
;   (cond
;    ((eq? dir 'up)
;     (set-whole-tree-zoom! Selected-tree (* (/ 1.2) (whole-tree-zoom Selected-tree))))
;    ((eq? dir 'down)
;     (set-whole-tree-zoom! Selected-tree (* 1.2 (whole-tree-zoom Selected-tree))))
;    ((eq? dir 'left)
;     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
;    ((eq? dir 'right)
;     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

