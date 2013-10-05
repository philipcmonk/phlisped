#lang racket

(require "../common.ss")
(require (except-in "../extractdata.ss" with))
(require "../graph.ss")
(require "../disp.ss")

(provide data search-bound-variables)

(define (search-bound-variables data text)
 (let ((regex (regexp text))
       (texts (cadddr (cddr data))))
  (filter (lambda (t) (regexp-match? regex (format "~a" (cadr t)))) texts)))

(define Search-text "")
(define Search-tree '())

(define (search event)
 (set! Search-text "")
 (enter-search-mode))

(define (handle-search event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((and (eq? c #\n) (send event get-control-down))
      (scroll-search-results)
      (show-search-tree get-rep)
      (send Thecanvas on-paint))
     ((and (char? c) (not (char-whitespace? c)) (not (char-iso-control? c)) (not (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\))))
      (set! Search-text (string-append Search-text (string (send event get-key-code))))
      (show-results))
     ((eq? c #\backspace)
      (set! Search-text (substring Search-text 0 (- (string-length Search-text) 1)))
      (show-results))
     ((eq? c #\return)
      (exit-search-mode))
     ((eq? c 'escape)
      (remove-search-tree)
      (send Thecanvas on-paint)
      (exit-search-mode))
     (#t '()))))

  (show-results ()
   (set-search-results (map triple->list (search-text Search-text)))
   (show-search-tree get-rep)
   (set-info Search-text)
   (send Thecanvas on-paint))))

(define (search-text text)
 (let* ((regex (regexp text))
        (texts (map (lambda (id) (graph-neighborhood-edge-forward G id "is written")) (graph-ids G)))
        (texts2 (filter (lambda (text) (not (null? text))) texts))
        (texts3 (map car texts2)))
  (filter (lambda (t) (regexp-match? regex (format "~a" (triple-end t)))) texts3)))

(define data
 (list
  #\/ search
  'search handle-search))