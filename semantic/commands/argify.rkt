#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with LINK1 LINK1PARENT LINK1ADDR))
(require "../graph.ss")
(require "../disp.ss")
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
          (has-child (member (triple parent-link2 "has child" link2) (graph-neighborhood-forward G parent-link2)))
          (parent-link2-t (if has-child (car has-child) '()))
          (is-defined-as (graph-neighborhood-edge-forward G link2 "is defined as"))
          (arg-id Next-id))
    (updater
     #:graph-changer     (lambda ()
                          (with
                           ((set-Next-id (+ 1 Next-id))
                            (add-arg-to-hijito)
                            (add-arg-to-call)
                            (convert-var-to-arg))
                  
                           (add-arg-to-hijito ()
                            (set-G (graph-append-edges G (list (triple LINK1 "has formal arg" arg-id) (triple arg-id "is written" 'arger) (triple arg-id "is reified as" (string->symbol (format "a~s" arg-id)))))))
                  
                           (add-arg-to-call ()
                            (let ((replacement (if (null? is-defined-as) link2 (triple-end (car is-defined-as)))))
                             (set-G (graph-append-edge G (triple LINK1PARENT "has child" replacement)))))
                  
                           (convert-var-to-arg ()
                            (set-G ((if (null? is-defined-as) swap-normal swap-var))))
                  
                           (swap-normal ()
                            (graph-replace-edges G parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) arg-id))))
                  
                           (swap-var ()
                            (graph-replace-edges G (car is-defined-as) (list (triple link2 "is defined as" arg-id))))))
     #:selection-updater (lambda ()
                          (exit-argify-mode)))))))

(define data
 (list
  #\a argify
  'argify handle-argify))

