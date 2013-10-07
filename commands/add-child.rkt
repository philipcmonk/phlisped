#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with))
(require "../core/graph.rkt")

(provide data add-child)

(define (add-child event)
 (let* ((id (selected-id Selected-tree))
        (is-defined-as (graph-neighborhood-edge-forward G id "is defined as")))
  (updater
   #:graph-changer     (lambda ()
                        (set-G (graph-prepend-edges G (list (triple id "has child" Next-id) (triple Next-id "is written" '-))))
                        (set-Next-id (+ 1 Next-id)))
   #:open-updater      (lambda ()
                        (for-all-trees
                         (lambda (tree)
                          (set-whole-tree-open! tree (set-union (whole-tree-open tree) (set (whole-tree-selection Selected-tree) (append (whole-tree-selection Selected-tree) (list 0))))))))
   #:selection-updater (lambda ()
                        (semantic-go 'down Selected-tree)))))

(define data
 (list #\( add-child))

