#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")

(provide data)

(define (interlocute-lambda function? event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
	(pgn (hash-ref G parent-id)))
  (updater
   #:graph-changer (lambda ()
                    (set-G (hash-set G parent-id (parent-gnode parent-id (gnode-name pgn) (replace id (list Next-id) (parent-gnode-childs pgn)) (cons Next-id (parent-gnode-vars pgn)))))
                    (set-G (hash-set G Next-id (if function?
						(function-gnode Next-id (gnode-name pgn) id '())
						(variable-gnode Next-id (gnode-name pgn) id))))
                    (set-Next-id (+ 1 Next-id)))
   #:open-updater  (lambda ()
                    (for-all-trees
                     (lambda (tree)
                      (set-whole-tree-open! tree (adjust-laddr-interlocute id (last (whole-tree-selection Selected-tree)) tree))))))))

(define data
 (list 
  #\v (curry interlocute-lambda #f)
  #\L (curry interlocute-lambda #t)))
