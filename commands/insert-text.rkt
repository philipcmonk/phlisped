#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" LINK1 LINK1PARENT LINK1ADDR INSERTTEXT))
(require "../core/gnode.rkt")
(require "../core/disp.rkt")
(require (only-in "add-sibling.rkt" add-sibling))
(require (only-in "add-child.rkt" add-child))
(require (only-in "search.rkt" search-bound-variables))

(provide data)

(define INSERTTEXT "")
(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

(define (insert-text event)
 (set! INSERTTEXT "")
 (enter-insert-mode))

(define (handle-insert event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((and (eq? c #\n) (send event get-control-down))
      (scroll-search-results)
      (show-search-tree get-rep)
      (send Thecanvas on-paint))
     ((and (char? c) (not (char-whitespace? c)) (not (char-iso-control? c)) (not (member c '(#\( #\) #\[ #\] #\{ #\} #\, #\' #\` #\; #\| #\\))))
      (set! INSERTTEXT (string-append INSERTTEXT (string (send event get-key-code))))
      (show-results))
     ((eq? c #\backspace)
      (set! INSERTTEXT (substring INSERTTEXT 0 (- (string-length INSERTTEXT) 1)))
      (show-results))
     ((eq? c #\space)
      (remove-search-tree)
      (if (and (send event get-shift-down) (not (null? Search-results)))
       (link-to (selected-id Selected-tree) (selected-parent-id Selected-tree) (caar Search-results))
       (write-text-to-graph))
      (exit-insert-mode)
      (add-sibling event)
      (insert-text event))
     ((eq? c #\()
      (remove-search-tree)
      (write-text-to-graph)
      (exit-insert-mode)
      (add-child event)
      (insert-text event))
     ((eq? c #\))
      (remove-search-tree)
      (write-text-to-graph)
      (exit-insert-mode)
      (semantic-go 'up Selected-tree)
      (insert-text event))
     ((eq? c #\return)
      (remove-search-tree)
      (if (and (send event get-shift-down) (not (null? Search-results)))
       (link-to (selected-id Selected-tree) (selected-parent-id Selected-tree) (caar Search-results))
       (write-text-to-graph))
      (exit-insert-mode))
     ((eq? c 'escape)
      (remove-search-tree)
      (send Thecanvas on-paint)
      (exit-insert-mode))
     (#t '()))))

  (show-results ()
   (set-search-results (search-bound-variables (node-data (utterance-node (whole-tree-selection-u Selected-tree))) INSERTTEXT))
   (show-search-tree get-rep)
   (set-info INSERTTEXT)
   (send Thecanvas on-paint))))

(define (link-to id1 parent-id1 id2)
 (let* ((gn2 (hash-ref G id2))
        (pgn1 (hash-ref G parent-id1)))
  (updater
   #:graph-changer (lambda ()
                    (cond
                     ((variable-gnode? gn2)
                      (set-G (hash-set G parent-id1
                              (cond
                               ((parent-gnode? pgn1) (parent-gnode parent-id1 (gnode-name pgn1) (replace id1 (list id2) (parent-gnode-childs pgn1)) (parent-gnode-vars pgn1)))
                               ((function-gnode? pgn1) (function-gnode parent-id1 (gnode-name pgn1) id2 (function-gnode-args pgn1)))
                               ((variable-gnode? pgn1) (variable-gnode parent-id1 (gnode-name pgn1) id2))
                               (#t '())))))
                     (#t '()))))))

(define (write-text-to-graph)
 (let* ((id (selected-id Selected-tree))
        (gn (hash-ref G id)))
  (updater
   #:graph-changer (lambda ()
                    (with
                     ((set-G
                       (hash-set G id
                        (cond
                         ((parent-gnode? gn) (parent-gnode (gnode-id gn) (get-insert-text) (parent-gnode-childs gn) (parent-gnode-vars gn)))
                         ((function-gnode? gn) (function-gnode (gnode-id gn) (get-insert-text) (variable-gnode-defined gn) (function-gnode-args gn)))
                         ((variable-gnode? gn) (variable-gnode (gnode-id gn) (get-insert-text) (variable-gnode-defined gn)))
                         ((argument-gnode? gn) (argument-gnode (gnode-id gn) (get-insert-text)))
                         ((terminal-gnode? gn) (terminal-gnode (gnode-id gn) (get-insert-text)))
                         (#t '())))))

                     (get-insert-text ()
                      (if (char-numeric? (car (string->list (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))) (string->number INSERTTEXT) (string->symbol (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))))))))

(define data
 (list
  #\i insert-text
  'insert handle-insert))

