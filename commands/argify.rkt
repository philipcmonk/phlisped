#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with LINK1 LINK1PARENT LINK1ADDR))
(require "../core/gnode.rkt")
(require "../core/disp.rkt")
(require (only-in "add-sibling.rkt" add-sibling))
(require (only-in "add-child.rkt" add-child))

(provide data)

(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

(define (argify event)
 (set! LINK1 (selected-id Selected-tree))
 (set! LINK1PARENT (selected-parent-id Selected-tree))
 (set! LINK1ADDR (whole-tree-selection Selected-tree))
 (enter-argify-mode))

(define (handle-argify event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((member c '(#\h #\j #\k #\l))
      ((hash-ref key-evs c) event))
     ((eq? c #\return)
      (make-arg))
     ((eq? c 'escape)
      (exit-argify-mode))
     (#t '()))))

  (make-arg ()
   (let* ((link2 (selected-id Selected-tree))
          (parent-link2 (selected-parent-id Selected-tree))
;          (has-child (member link2 (hash-ref G parent-link2)))
;          (parent-link2-t (if has-child (car has-child) '()))
;          (is-defined-as (graph-neighborhood-edge-forward G link2 "is defined as"))
          (arg-id Next-id))
    (updater
     #:graph-changer     (lambda ()
                          (let ((l2gn (hash-ref G link2))
				(pl2gn (hash-ref G parent-link2))
				(l1gn (hash-ref G LINK1)))
                           (with
                            ((set-Next-id (+ 1 Next-id))
                             (add-arg-to-hijito)
;                             (add-arg-to-call)
                             (convert-var-to-arg))
                   
                            (add-arg-to-hijito ()
			     (set-G (hash-set G LINK1 (function-gnode LINK1 (gnode-name l1gn) (variable-gnode-defined l1gn) (append (function-gnode-args l1gn) (list arg-id)))))
			     (set-G (hash-set G arg-id (argument-gnode arg-id 'arger))))
;                             (set-G (graph-append-edges G (list (triple LINK1 "has formal arg" arg-id) (triple arg-id "is written" 'arger) (triple arg-id "is reified as" (string->symbol (format "a~s" arg-id)))))))
                   
                            (add-arg-to-call ()
			     '())
;                             (let ((replacement (if (variable-gnode? l2gn) (variable-gnode-defined l2gn) link2)))
;                              (set-G (graph-append-edge G (triple LINK1PARENT "has child" replacement)))))
                   
                            (convert-var-to-arg ()
                             (set-G ((if (variable-gnode? l2gn) swap-var swap-normal))))
                   
                            (swap-normal ()
                             (hash-set G parent-link2 (parent-gnode parent-link2 (gnode-name pl2gn) (replace link2 (list arg-id) (parent-gnode-childs pl2gn)) (parent-gnode-vars pl2gn))))
;                             (graph-replace-edges G parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) arg-id))))
                   
                            (swap-var ()
			     (hash-set G link2 (variable-gnode link2 (gnode-name l2gn) arg-id))))))
;                             (graph-replace-edges G (variable-gnode-defined l2gn) (list (triple link2 "is defined as" arg-id)))))))
     #:selection-updater (lambda ()
                          (exit-argify-mode)))))))

(define data
 (list
  #\a argify
  'argify handle-argify))

