#lang racket

(require "graph.ss" "find.ss" "disptree.ss")
(require racket/set)

(provide Thecanvas Info paint-info)

(define FILENAME "graph.ss")
(define GRFILE "data")

(define X 0)
(define Y 0)
(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define Next-id 0)

(define (extract code)
 (append
  (flatten
   (list
    (if (pair? code)
     (begin
      (triple (car code) "is a" "sexp")
      (if (list? (cdr code))
       (list
        (map (lambda (child) (triple (car code) "has child" (car child))) (cdr code))
        (triple (car code) "has tail child" (car (last code)))
        (map (lambda (child) (if (< (string-length (format "~s" (cadr code))) (string-length (format "~s" (cdr child))))
                              (triple (car code) "is longer than" (car child)) '())) (cdr code)))
       (list
        (triple (car code) "is written" (cdr code))
        (triple (car code) "has property" "terminal"))))
     '())))
  (if (list? code) (flatten (map extract code)) '())))

(define (idify code)
 (let ((id Next-id))
  (set! Next-id (+ 1 Next-id))
  (if (null? code)
   (cons id '__null)
   (if (list? code)
    (cons id (map idify code))
    (cons id code)))))

;(define G
; (graph
;  '()
;  (extract
;   (idify
;    (call-with-input-file FILENAME
;     (lambda (f)
;      (read-accept-reader #t)
;      (define (in rem)
;       (let ((x (read rem)))
;        (if (eof-object? x)
;         '(end)
;         (cons x (in rem)))))
;      (in f)))))))

;(define G
; (string->graph (file->string GRFILE)))

(define G
 (call-with-input-file GRFILE (lambda (f) (read f))))

(define (get-rep id child-fun)
 (cons id (get-written id child-fun)))
; (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
;  (if (null? nbhd)
;   (cons id '())
;   (cons id (triple-end (car nbhd))))))

(define (get-written id child-fun)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
  (if (null? nbhd)
   (get-written (caar (child-fun (cons id '()))) child-fun)
   (triple-end (car nbhd)))))

(display (graph->string G))
;(call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write G f)))

;(display (cadr (graph-edges G)))
;(display "\n")
;(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has tail child")) (triple-start (car (graph-edges G)))))
;(display "\n")

;(display "\n")
;(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")) (triple-start (seventh (graph-edges G)))))
;(display "\n")

;(display (extract (idify (call-with-input-file FILENAME
; (lambda (f)
;  (read-accept-reader #t)
;  (define (in rem)
;   (let ((x (read rem)))
;    (if (eof-object? x)
;     '(end)
;     (cons x (in rem)))))
;  (in f))))))
;(display "\n")

(define child-fun (lambda (a) (
 (compose
  (curry map (compose (curryr get-rep child-fun) triple-end))
  (compose
   (curryr (curry graph-neighborhood-edge-forward G) "has child")
   car))
  a)))

(define (root->list root child-fun)
 (if (null? (cdr root))
  (map (curryr root->list child-fun) (child-fun root))
  (format "~s" (cdr root))))


(let ((id (triple-start (car (graph-edges G)))))
; (display (root->list (cons id '()) child-fun))
; (newline)
 (display-on-screen 0 30 (round (/ WIDTH 2)) (- HEIGHT 30) (cons id '())
  child-fun))

;(display-on-screen (round (/ WIDTH 2)) 30 (round (/ WIDTH 2)) (- HEIGHT 30) (triple-start (car (graph-edges G))) (compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")))

;(round (/ (* 2 WIDTH) 3))

;(send win show #t)

;(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")) "(set-for-each (graph-edges g) display-triple)"))
