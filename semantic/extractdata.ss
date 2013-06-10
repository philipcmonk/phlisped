#lang racket

(require "graph.ss" "find.ss" "disptree.ss")

(define FILENAME "graph.ss")

(define X 0)
(define Y 0)
(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define (extract code)
 (append
  (flatten
   (list
    (triple code "is a" "sexp")
    (if (list? code)
     (list
      (map (lambda (child) (triple code "has child" child)) code)
      (triple code "has tail child" (last code))
      (map (lambda (child) (if (< (string-length (format "~s" (car code))) (string-length (format "~s" child)))
                            (triple code "is longer than" child) '())) code))
     '())))
  (if (list? code) (flatten (map extract code)) '())))

(define G
 (graph
  (set)
  (extract
   (call-with-input-file FILENAME
    (lambda (f)
     (read-accept-reader #t)
     (define (in rem)
      (let ((x (read rem)))
       (if (eof-object? x)
        '(end)
        (cons x (in rem)))))
     (in f))))))

(display (graph->string G))

;(display (cadr (graph-edges G)))
;(display "\n")
;(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has tail child")) (triple-start (car (graph-edges G)))))
;(display "\n")

(display "\n")
(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")) (triple-start (seventh (graph-edges G)))))
(display "\n")

(display-on-screen X Y WIDTH HEIGHT (triple-start (car (graph-edges G))) (compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")))

;(send win show #t)

;(display ((compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward G) "has child")) "(set-for-each (graph-edges g) display-triple)"))
