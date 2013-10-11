#lang racket

(require "../core/common.rkt")
(require "../core/commands-common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")

(provide data add-child)

(define add-child
 (event
  (lambda (g id parent-id)
   ((compose
     (curryr graph-add-terminal-gnode Next-id)
     (curryr graph-add-child-beg id Next-id))
    g))
  1
  (lambda (tree id parent-id)
   (set-union
    (set (whole-tree-selection Selected-tree) (append (whole-tree-selection Selected-tree) (list 0)))
    (list->set
     (set-map
      (whole-tree-open tree)
      (curry adjust-laddr Next-id id
             0
             (whole-tree-utterance-tree tree))))))
  (lambda ()
   (semantic-go 'down Selected-tree))))

;(define (add-child event)
; (let* ((id (selected-id Selected-tree)))
;  (updater
;   #:graph-changer     (lambda ()
;                        (let ((gn (hash-ref G id)))
;                         (set-G (hash-set G id (parent-gnode id (gnode-name gn) (cons Next-id (if (parent-gnode? gn) (parent-gnode-childs gn) '())) (if (parent-gnode? gn) (parent-gnode-vars gn) '()))))
;			 (set-G (hash-set G Next-id (terminal-gnode id '-)))
;                         (set-Next-id (+ 1 Next-id))))
;   #:open-updater      (lambda ()
;                        (for-all-trees
;                         (lambda (tree)
;                          (set-whole-tree-open! tree (set-union (whole-tree-open tree) (set (whole-tree-selection Selected-tree) (append (whole-tree-selection Selected-tree) (list 0))))))))
;   #:selection-updater (lambda ()
;                        (semantic-go 'down Selected-tree)))))

(define data
 (list #\( (event-wrapper add-child)))

