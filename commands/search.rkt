#lang racket

(require "../core/common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")
(require "../core/disp.rkt")

(provide data search-bound-variables)

(define (search-bound-variables data text)
 (let ((regex (regexp text))
       (texts (cadddr (cddr data))))
  (filter (lambda (t) (regexp-match? regex (format "~a" (cadr t)))) texts)))

(define Search-text "")

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
   (set-search-results (search-text Search-text))
   (show-search-tree get-rep)
   (set-info Search-text)
   (send Thecanvas on-paint))))

(define (search-text text)
 (let* ((regex (regexp text))
        (texts (hash-map (hash-remove G 'next-id)  (lambda (id gn) (list id '() (gnode-name gn)))))
        (texts2 (filter (lambda (text) (not (null? text))) texts)))
  (filter (lambda (t) (regexp-match? regex (format "~a" (caddr t)))) texts2)))

(define data
 (list
  '(#\/ enter-search) search
  '(search) handle-search))
