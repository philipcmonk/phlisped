#lang racket

(require "../core/common.rkt")
(require "../core/commands-common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")

(provide data add-sibling)

(define add-sibling-ev
 (event
  (lambda (g id parent-id)
   ((compose
     (curryr graph-add-terminal-gnode Next-id)
     (curryr graph-add-child-after parent-id id Next-id))
    g))
  1
  (lambda (tree id parent-id)
   (set-union
    (list->set
     (set-map
      (whole-tree-open tree)
      (curry adjust-laddr id parent-id
             (last (whole-tree-selection Selected-tree))
             (whole-tree-utterance-tree tree))))
    (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree))))))))
  (lambda ()
   (semantic-go 'right Selected-tree))))

;(define (add-sibling event)
; (let* ((id (selected-id Selected-tree))
;        (parent-id (selected-parent-id Selected-tree)))
;  (updater
;   #:graph-changer     (lambda ()
;                        (let ((gn (hash-ref G id))
;			      (pgn (hash-ref G parent-id)))
;			 (set-G (hash-set G parent-id (parent-gnode parent-id (gnode-name pgn) (replace id (list id Next-id) (parent-gnode-childs pgn)) (parent-gnode-vars pgn))))
;			 (set-G (hash-set G Next-id (terminal-gnode Next-id '-)))
;                         (set-Next-id (+ 1 Next-id))))
;   #:open-updater      (lambda ()
;                        (for-all-trees
;                         (lambda (tree)
;                          (set-whole-tree-open! tree
;                                                (set-union
;                                                 (list->set
;                                                  (set-map
;                                                   (whole-tree-open tree)
;                                                   (curry adjust-laddr id parent-id
;                                                          (last (whole-tree-selection Selected-tree))
;                                                          (whole-tree-utterance-tree tree))))
;                                                 (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree)))))))))))
;   #:selection-updater (lambda () (semantic-go 'right Selected-tree)))))

(define add-sibling (event-wrapper add-sibling-ev))

(define data
 (list #\space add-sibling))

