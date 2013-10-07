#lang racket

; Remove vertex list

(require racket/set)

(provide (except-out (all-defined-out) replace))

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

(define (triple->list t)
 (list (triple-start t) (triple-edge t) (triple-end t)))

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

(define (graph-neighborhood-forward g v)
 (car (hash-ref g v)))

(define (graph-neighborhood-edge-forward g v edge)
 (filter (lambda (e) (equal? edge (triple-edge e))) (car (hash-ref g v '(() . ())))))

(define (graph-neighborhood-backward g v)
 (cdr (hash-ref g v)))

(define (graph-neighborhood-edge-backward g v edge)
 (filter (lambda (e) (equal? edge (triple-edge e))) (cdr (hash-ref g v '(() . ())))))

(define (graph-append-edges g edges)
 (foldl (lambda (e h) (graph-append-edge h e)) g edges))

(define (graph-append-edge g edge)
 (let* ((v1 (triple-start edge))
        (v2 (triple-end edge))
        (h (hash-set g v1 (cons (append (car (hash-ref g v1 '(() . ()))) (list edge)) (cdr (hash-ref g v1 '(() ()))))))
        (hh (hash-set h v2 (cons (car (hash-ref g v2 '(() . ()))) (append (cdr (hash-ref g v2 '(() . ()))) (list edge))))))
  hh))

(define (graph-prepend-edges g edges)
 (foldl (lambda (e h) (graph-prepend-edge h e)) g edges))

(define (graph-prepend-edge g edge)
 (let* ((v1 (triple-start edge))
        (v2 (triple-end edge))
        (h (hash-set g v1 (cons (append (list edge) (car (hash-ref g v1))) (cdr (hash-ref g v1)))))
        (hh (hash-set h v2 (cons (car (hash-ref g v2 '(() . ()))) (append (cdr (hash-ref g v2 '(() . ()))) (list edge))))))
  hh))

(define (graph-replace-edges g edge edges)
 (let* ((v1 (triple-start edge))
        (v2 (triple-end edge))
        (h (hash-set g v1 (cons (replace edge edges (car (hash-ref g v1))) (cdr (hash-ref g v1)))))
        (hh (hash-set h v2 (cons (car (hash-ref h v2 '(() . ()))) (remove edge (cdr (hash-ref h v2 '(() . ()))))))))
  (foldl (lambda (e hhh)
          (let* ((vv2 (triple-end e))
                 (hhhh (hash-set hhh vv2 (cons (car (hash-ref hhh vv2 '(() . ()))) (append (cdr (hash-ref hhh vv2 '(() . ()))) (list e))))))
           hhhh))
         hh
         edges)))

(define (replace t1 t2s es)
 (append (takef es (negate (curry equal? t1))) t2s (cdr (member t1 es))))

(define (graph-remove-edge g edge)
 (let* ((v1 (triple-start edge))
        (v2 (triple-end edge))
        (h (hash-set g v1 (cons (remove edge (car (hash-ref g v1))) (cdr (hash-ref g v1)))))
        (hh (hash-set h v2 (cons (car (hash-ref h v2 '(() . ()))) (remove edge (cdr (hash-ref h v2 '(() . ()))))))))
  hh))

