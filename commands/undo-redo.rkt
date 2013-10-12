#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")

(provide data)

(define (undo-pop event)
 (if (null? UNDOSTACK)
  '()
  (begin
   (set-REDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) REDOSTACK))
   (set-G (caar UNDOSTACK))
   (set-whole-tree-open! Selected-tree (cadar UNDOSTACK))
   (set-whole-tree-selection! Selected-tree (caddar UNDOSTACK))
   (set-UNDOSTACK (cdr UNDOSTACK))
   (update-data)
   (update-childfuncs child-fun))))

(define (redo-pop event)
 (if (null? REDOSTACK)
  '()
  (begin
   (set-UNDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) UNDOSTACK))
   (set-G (caar REDOSTACK))
   (set-whole-tree-open! Selected-tree (cadar REDOSTACK))
   (set-whole-tree-selection! Selected-tree (caddar REDOSTACK))
   (set-REDOSTACK (cdr REDOSTACK))
   (update-data)
   (update-childfuncs child-fun))))

(define data
 (list
  '(#\u undo) undo-pop
  '(#\R redo) redo-pop))


