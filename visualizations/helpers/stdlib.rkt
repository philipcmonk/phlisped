#lang racket

(require "../../core/common.rkt")
(require "def-painter.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (when-space-for-text u tree text-drawer)
 (when (and
      (<= (/ (- (cartesian-utterance-text-w u) PADDING) (whole-tree-zoom tree)) (cartesian-utterance-w u))
      (<= (/ (cartesian-utterance-text-h u) (whole-tree-zoom tree)) (cartesian-utterance-h u)))
  text-drawer))

(define (draw-rectangle-u u tree)
 (draw-rectangle (cdr (utterance-clr u)) (cartesian-utterance-x u) (cartesian-utterance-y u) (cartesian-utterance-w u) (cartesian-utterance-h u)))

(define (straight-text u tree)
 ((node-text-func (utterance-node u)) (utterance-node u)))

(define (rectangle-drawer u tree)
 (generic-drawer u tree #:drawer draw-rectangle-u #:text straight-text))

(define (generic-drawer u tree #:drawer (drawer draw-rectangle-u) #:text (text straight-text))
 (drawer u tree)
 (when-space-for-text u tree
  (draw-text (text u tree) (center-x u tree) (center-y u tree) (car (utterance-clr u)))))

(define (center-x u tree)
 (center (cartesian-utterance-x u) (cartesian-utterance-w u) (- (cartesian-utterance-text-w u) PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))

(define (center-y u tree)
 (+ (cartesian-utterance-text-h u) -3 (center (cartesian-utterance-y u) (cartesian-utterance-h u) (- (cartesian-utterance-text-h u) PADDING) (- (whole-tree-offset-y tree)) (whole-tree-h tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utterance-generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cartesian-utterance-generator n tree)
 (generic-utterance-generator n tree cartesian-utterance
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) (+ data (cartesian-utterance-w res)))
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) (+ data CELLHEIGHT))
   (lambda (data chs res n) data)
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (max (box-width ((node-text-func n) n)) (apply + (map cartesian-utterance-w chi)))))
  (list CELLHEIGHT
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (box-width ((node-text-func n) n))))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (box-height ((node-text-func n) n))))))

(struct other-v11n-utterance cartesian-utterance (total-height))

(define (linear-vertical-utterance-generator n tree)
 (generic-utterance-generator n tree other-v11n-utterance
  (list 0
   (lambda (data chs arg n) (+ data 10))
   (lambda (data chs res n) data)
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) (+ CELLHEIGHT data))
   (lambda (data chs res n) (+ data (other-v11n-utterance-total-height res)))
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (box-width ((node-text-func n) n))))
  (list CELLHEIGHT
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) data))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (box-width ((node-text-func n) n))))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (box-height ((node-text-func n) n))))
  (list 0
   (lambda (data chs arg n) data)
   (lambda (data chs res n) data)
   (lambda (data chi n) (apply + CELLHEIGHT (map other-v11n-utterance-total-height chi))))))

(define (generic-utterance-generator n tree utterance-constructor . chrs)
 (let ((inits (map car chrs))
       (make-children (map cadr chrs))
       (updates (map caddr chrs))
       (finals (map cadddr chrs)))
  (let node->utterance ((n n) (tree tree) (characteristics inits))
   (let ((children
          (if (closed? n tree)
           '()
           (car
            (foldl
             (lambda (arg data)
              (let ((res (node->utterance
                          arg
                          tree
                          (map (curryr apply (list characteristics arg n)) make-children (cdr data)))))
               (cons (append (car data) (list res)) (map (curryr apply (list characteristics res n)) updates (cdr data)))))
	     (cons '() characteristics)
	     (node-args n))))))
    (apply
     utterance-constructor
     n
     children
     (get-color n tree)
     (map (curryr apply children (list n)) finals characteristics))))))

(define (horiz-scroller dir event)
 (cond
  ((eq? dir 'up)
   (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
  ((eq? dir 'down)
   (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
  ((eq? dir 'left)
   (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
  ((eq? dir 'right)
   (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))

(define (vert-scroller dir event)
 (cond
  ((eq? dir 'up)
   (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
  ((eq? dir 'down)
   (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))
  ((eq? dir 'left)
   (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
  ((eq? dir 'right)
   (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))))
