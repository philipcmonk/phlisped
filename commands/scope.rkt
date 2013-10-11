#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" LINK1 LINK1PARENT LINK1ADDR))
(require "../core/gnode.rkt")
(require "../core/disp.rkt")

(provide data)

(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

(define (set-scope event)
 (set! LINK1 (selected-id Selected-tree))
 (set! LINK1ADDR (whole-tree-selection Selected-tree))
 (enter-scope-mode))

(define (handle-scope event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((member c '(#\h #\j #\k #\l))
      ((hash-ref key-evs c) event))
     ((eq? c #\return)
      (make-scope))
     ((eq? c 'escape)
      (exit-scope-mode))
     (#t '()))))

  (make-scope ()
   (let* ((link2 (selected-id Selected-tree)))
    (updater
     #:graph-changer     (lambda ()
                          (let ((l2gn (hash-ref G link2)))
			   (purge-var LINK1)
			   (set-G (hash-set G link2 (parent-gnode link2 (gnode-name l2gn) (parent-gnode-childs l2gn) (cons LINK1 (parent-gnode-vars l2gn)))))))
     #:selection-updater (lambda ()
                          (exit-scope-mode)))))))

(define (purge-var id)
 (hash-for-each
  G
  (lambda (id-in gn)
   (if (parent-gnode? gn)
    (set-G (hash-set G id-in (parent-gnode id-in (gnode-name gn) (parent-gnode-childs gn) (remove id (parent-gnode-vars gn)))))
    gn))))

(define data
 (list
  #\s set-scope
  'scope handle-scope))

