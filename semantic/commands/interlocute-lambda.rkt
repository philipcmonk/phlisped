#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with))
(require "../graph.ss")

(provide data)

(define (interlocute-lambda function? event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (has-child (member (triple parent-id "has child" id) (graph-neighborhood-forward G parent-id)))
        (parent-link-t (if has-child (car has-child) '())))
  (updater
   #:graph-changer (lambda ()
                    (set-G (graph-replace-edges G parent-link-t
                                                 (if function?
                                                  (list (triple parent-id (triple-edge parent-link-t) Next-id))
                                                  (list (triple parent-id (triple-edge parent-link-t) Next-id)))))
                    (set-G (graph-append-edges G (if function?
                                                   (list (triple Next-id "is defined as" id) (triple Next-id "has env" parent-id) (triple Next-id "is function" id))
                                                   (list (triple Next-id "is defined as" id) (triple Next-id "has env" parent-id)))))
                    (set-Next-id (+ 1 Next-id)))
   #:open-updater  (lambda ()
                    (for-all-trees
                     (lambda (tree)
                      (set-whole-tree-open! tree (adjust-laddr-interlocute id (last (whole-tree-selection Selected-tree)) tree))))))))

(define data
 (list 
  #\v (curry interlocute-lambda #f)
  #\L (curry interlocute-lambda #t)))