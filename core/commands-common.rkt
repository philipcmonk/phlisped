#lang racket

(require "common.rkt")
(require "disp.rkt")
(require "extractdata.rkt")
(require "gnode.rkt")

(provide (all-defined-out))

(struct event (graph-changer ids-consumed open-updater selection-updater))

(define (graph-add-terminal-gnode g id)
 (hash-set g id (terminal-gnode id '-)))

(define (graph-add-child-beg g id cid)
 (let ((gn (hash-ref g id)))
  (hash-set g id (parent-gnode id (gnode-name gn) (cons cid (if (parent-gnode? gn) (parent-gnode-childs gn) '())) (if (parent-gnode? gn) (parent-gnode-vars gn) '())))))
        
(define (graph-add-child-after g id cid nid)
 (let ((gn (hash-ref g id)))
  (hash-set g id (parent-gnode id (gnode-name gn) (replace cid (list cid nid) (if (parent-gnode? gn) (parent-gnode-childs gn) '())) (if (parent-gnode? gn) (parent-gnode-vars gn) '())))))

(define (event-wrapper ev)
 (lambda (e)
  (let* ((id (selected-id Selected-tree))
         (parent-id (if (zero? id) 0 (selected-parent-id Selected-tree))))
   (updater
    #:graph-changer
    (lambda ()
     (set-G ((event-graph-changer ev) G id parent-id )))
    #:open-updater
    (lambda ()
     (for-all-trees (lambda (tree) (set-whole-tree-open! tree ((event-open-updater ev) tree id parent-id)))))
    #:selection-updater
    (lambda ()
     ((event-selection-updater ev))
     (set-Next-id (+ Next-id (event-ids-consumed ev))))))))

