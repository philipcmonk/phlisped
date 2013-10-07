#lang racket

(require "graph.rkt")

; Return all triples for which (pred t) returns true.
(define (graph-filter g pred)
 (filter pred (graph-edges g)))

; Return all triples joined to 'v'.
; This is graph-neighborhood-foward union graph-neighborhood-backward.
(define (graph-neighborhood g v)
 (append (graph-neighborhood-forward g v) (graph-neighborhood-backward g v)))

; Return all triples with 'v' as the start.
(define (graph-neighborhood-forward g v)
 (graph-filter g (lambda (t) (equal? (triple-start t) v))))

; Return all triples with 'v' as the end
(define (graph-neighborhood-backward g v)
 (graph-filter g (lambda (t) (equal? (triple-end t) v))))

; Start at an initial point and return all triples in the tree created by following
; a particular type of edge.  endpoint1 is a predicate that returns true if the triple
; is in the tree, and endpoint2 is a predicate that returns which element to use as
; the next initial element.  This is an auxiliary function -- most will want to use
; graph-follow-forward or graph-follow-backward.
(define (graph-follow-aux g v edge endpoint1 endpoint2)
 (let* ((matches (graph-neighborhood-edge-aux g v edge endpoint1)))
  (append
   matches
   (flatten (map (lambda (t) (graph-follow-aux g (endpoint2 t) edge endpoint1 endpoint2)) matches)))))

; Return all triples connected by 'edge' to 'v'.
; That is, the tree created by recursively following 'edge'.  This is
; graph-follow-forward union graph-follow-backward.
(define (graph-follow g v edge)
 (append (graph-follow-forward g v edge) (graph-follow-backward g v edge)))

; Return all triples connected by 'edge' to 'v' in the forward direction.
; That is, the tree created by recursively following 'edge' in the forward
; direction.
(define (graph-follow-forward g v edge)
 (graph-follow-aux g v edge triple-start triple-end))

; Return all triples connected by 'edge' to 'v' in the backward direction.
; That is, the tree created by recursively following 'edge' in the backward
; direction.
(define (graph-follow-backward g v edge)
 (graph-follow-aux g v edge triple-end triple-start))

; Auxiliary function.  Return all triples joined to 'v' by following 'edge' where
; pred determines which endpoint to use.
(define (graph-neighborhood-edge-aux g v edge pred)
 (graph-filter g (lambda (t) (and (equal? (triple-edge t) edge) (eq? (pred t) v)))))

; Find all triples joined by 'edge' to 'v'.
; This is graph-neighborhood-edge-forward union graph-neighborhood-edge-backward.
(define (graph-neighborhood-edge g v edge)
 (append (graph-neighborhood-edge-forward g v edge) (graph-neighborhood-edge-backward g v edge)))

; Find all triples joined by 'edge' to 'v' in the forward direction.
(define (graph-neighborhood-edge-forward g v edge)
 (graph-neighborhood-edge-aux g v edge triple-start))

; Find all triples joined by 'edge' to 'v' in the backward direction.
(define (graph-neighborhood-edge-backward g v edge)
 (graph-neighborhood-edge-aux g v edge triple-end))

(define (graph-edge-neighborhood g edge)
 (graph-filter g (lambda (t) (equal? (triple-edge t) edge))))

(provide (all-defined-out))

