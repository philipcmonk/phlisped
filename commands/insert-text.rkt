#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with LINK1 LINK1PARENT LINK1ADDR INSERTTEXT))
(require "../core/graph.rkt")
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
       (link-to (selected-id Selected-tree) (caar Search-results))
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
       (link-to (selected-id Selected-tree) (caar Search-results))
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

(define (link-to id1 id2)
 (let* ((is-defined-as-2 (graph-neighborhood-edge-forward G id2 "is defined as"))
        (has-child (graph-neighborhood-edge-backward G id1 "has child"))
        (is-defined-as-1 (graph-neighborhood-edge-backward G id1 "is defined as"))
        (parent-id1-t (if (not (null? has-child)) (car has-child) (if (not (null? is-defined-as-1)) (car is-defined-as-1) '()))))
  (updater
   #:graph-changer (lambda ()
                    (cond
                     ((not (null? is-defined-as-2))
                      (set-G (graph-replace-edges G parent-id1-t (list (triple (triple-start parent-id1-t) (triple-edge parent-id1-t) id2)))))
                     (#t '()))))))

(define (write-text-to-graph)
 (let* ((id (selected-id Selected-tree))
        (is-written (is-written-t id))
        (is-named (graph-neighborhood-edge-forward G id "is named")))
  (updater
   #:graph-changer (lambda ()
                    (with
                     ((set-G (if is-written
                               (set-written)
                               (if (null? is-named)
                                (add-name)
                                (set-name)))))

                     (get-insert-text ()
                      (if (char-numeric? (car (string->list (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))) (string->number INSERTTEXT) (string->symbol (if (eq? "" INSERTTEXT) "-" INSERTTEXT))))

                     (set-written ()
                      (graph-replace-edges G is-written (list (triple id "is written" (get-insert-text)))))

                     (add-name ()
                      (graph-append-edge G (triple id "is named" (get-insert-text))))

                     (set-name ()
                      (graph-replace-edges G (car is-named) (list (triple id "is named" (get-insert-text))))))))))

(define data
 (list
  #\i insert-text
  'insert handle-insert))
