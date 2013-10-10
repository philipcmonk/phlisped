#lang racket

(require "../../core/common.rkt")
(require "def-painter.rkt")

(provide (all-defined-out))

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

