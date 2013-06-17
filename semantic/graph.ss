#lang racket

; Remove vertex list

(require racket/set)

(struct graph (vertices edges) #:prefab)
(struct triple (start edge end) #:prefab)

(define (string->graph str)
 (let* ((lines (filter (lambda (line) (not (equal? line ""))) (regexp-split #rx"\n" str)))
	(edges (map (lambda (line) (apply triple (regexp-split #rx"\t" line))) lines)))
  (graph
   (apply set-union (map (lambda (e) (set (triple-start e) (triple-end e))) edges))
   edges)))

(define (graph->string g)
 (apply string-append (map (lambda (t) (string-append (format "~s" (triple-start t)) "\t" (format "~s" (triple-edge t)) "\t" (format "~s" (triple-end t)) "\n")) (graph-edges g))))

(define (display-graph g)
 (set-for-each (graph-vertices g) (lambda (t) (display t) (display "\t")))
 (newline)
 (newline)
 (for-each display-triple (graph-edges g)))

(define (display-triple t)
 (display (triple-start t)) (display " --> ") (display (triple-edge t)) (display " --> ") (display (triple-end t)) (newline))

(provide (all-defined-out))

(define (create-triples v1 e v2)
 (flatten 
  (let* ((len (cond
	       ((list? v1) (length v1))
	       ((list? e) (length e))
	       ((list? v2) (length v2))
	       (#t 1))))
   (map
    (lambda (vv1 ee vv2)
     (if (or (list? vv1) (list? ee) (list? vv2))
      (create-triples vv1 ee vv2)
      (triple vv1 ee vv2)))
    (if (list? v1) v1 (make-list len v1))
    (if (list? e) e (make-list len e))
    (if (list? v2) v2 (make-list len v2))))))

