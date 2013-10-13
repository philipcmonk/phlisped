#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/disp.rkt")
(require "../core/gnode.rkt")

(provide data)

(define (undo-pop event)
 (if (null? UNDOSTACK)
  '()
  (begin
   (set-REDOSTACK (cons (state G (copy-trees) (get-selected-tree-index)) REDOSTACK))
   (set-G (state-graph (car UNDOSTACK)))
   (set-Trees (state-trees (car UNDOSTACK)))
   (set-selected-tree (list-ref Trees (state-selected-tree (car UNDOSTACK))))
   (set-UNDOSTACK (cdr UNDOSTACK))
   (update-data)
   (update-childfuncs child-fun))))

(define (redo-pop event)
 (if (null? REDOSTACK)
  '()
  (begin
   (set-UNDOSTACK (cons (state G (copy-trees) (get-selected-tree-index)) UNDOSTACK))
   (set-G (state-graph (car REDOSTACK)))
   (set-Trees (state-trees (car REDOSTACK)))
   (set-selected-tree (list-ref Trees (state-selected-tree (car REDOSTACK))))
   (set-REDOSTACK (cdr REDOSTACK))
   (update-data)
   (update-childfuncs child-fun))))

(define data
 (list
  '(#\u undo) undo-pop
  '(#\R redo) redo-pop))


