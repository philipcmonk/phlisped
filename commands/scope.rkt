#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with LINK1 LINK1PARENT LINK1ADDR))
(require "../core/gnode.rkt")
(require "../core/disp.rkt")

(provide data)

(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

(define data '())
;(define (set-scope event)
; (set! LINK1 (selected-id Selected-tree))
; (set! LINK1ADDR (whole-tree-selection Selected-tree))
; (enter-scope-mode))
;
;(define (handle-scope event)
; (with
;  ((let ((c (send event get-key-code)))
;    (cond
;     ((member c '(#\h #\j #\k #\l))
;      ((hash-ref key-evs c) event))
;     ((eq? c #\return)
;      (make-scope))
;     ((eq? c 'escape)
;      (exit-scope-mode))
;     (#t '()))))
;
;  (make-scope ()
;   (let* ((link2 (selected-id Selected-tree)))
;    (updater
;     #:graph-changer     (lambda ()
;                          (let ((has-env (graph-neighborhood-edge-forward G LINK1 "has env")))
;                           (if (not (null? has-env))
;                            (set-G (graph-replace-edges G (car has-env) (list (triple LINK1 "has env" link2))))
;                            '())))
;     #:selection-updater (lambda ()
;                          (exit-scope-mode)))))))
;
;(define data
; (list
;  #\s set-scope
;  'scope handle-scope))

