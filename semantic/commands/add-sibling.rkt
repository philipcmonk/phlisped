#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with))
(require "../graph.ss")

(provide data add-sibling)

(define (add-sibling event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree)))
  (updater
   #:graph-changer     (lambda ()
                        (set-G (graph-replace-edges G (triple parent-id "has child" id) (list (triple parent-id "has child" id) (triple parent-id "has child" Next-id))))
                        (set-G (graph-append-edge G (triple Next-id "is written" '-)))
                        (set-Next-id (+ 1 Next-id)))
   #:open-updater      (lambda ()
                        (for-all-trees
                         (lambda (tree)
                          (set-whole-tree-open! tree
                                                (set-union
                                                 (list->set
                                                  (set-map
                                                   (whole-tree-open tree)
                                                   (curry adjust-laddr id parent-id
                                                          (last (whole-tree-selection Selected-tree))
                                                          (whole-tree-utterance-tree tree))))
                                                 (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree)))))))))))
   #:selection-updater (lambda () (semantic-go 'right Selected-tree)))))

(define data
 (list #\space add-sibling))

