#lang racket

(require "graph.ss" "find.ss" "disp.ss")
(require racket/set)

(provide Thecanvas Info (all-defined-out))

(define FILENAME "extractdata.ss")
(define GRFILE "data2")

(define NEWCODE #f)

(define X 0)
(define Y 0)
(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define Next-id 0)

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(define G
 (with
  ((if NEWCODE
    (graph
     '()
     (extract
      (idify
       (call-with-input-file FILENAME
        (lambda (f)
         (read-accept-reader #t)
         (define (in rem)
          (let ((x (read rem)))
           (if (eof-object? x)
            '(end)
            (cons x (in rem)))))
         (in f))))))
    (call-with-input-file GRFILE (lambda (f) (read f)))))

  (extract (code)
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

  (idify (code)
   (let ((id Next-id))
    (set! Next-id (+ 1 Next-id))
    (if (null? code)
     (cons id '__null)
     (if (list? code)
      (cons id (map idify code))
      (cons id code)))))
  ))


(define (graph->file g)
 (call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write g f))))

(display (graph->string G))
(if NEWCODE
 (graph->file G)
 '())

(define (reify g id)
 (let ((has-child (graph-neighborhood-edge-forward G id "has child"))
       (is-call-to (graph-neighborhood-edge-forward G id "is call to"))
       (is-written (graph-neighborhood-edge-forward G id "is written"))
       (has-scope (graph-neighborhood-edge-backward G id "has scope")))
  (string-append
   (if (null? has-scope)
    ""
    (apply string-append
     (map ; go forwards on (triple-start t) to get argnames
      (lambda (t) (format "(define (f~s ~a) ~a)" (triple-start t) (string-join (map (compose (curry format "~s") triple-end) (graph-neighborhood-edge-forward G (triple-start t) "has argname")) " ") (reify g (triple-start t))))
      has-scope)))
   (if (not (null? is-written))
    (format "~s" (triple-end (car is-written)))
    (if (not (null? has-child))
     (string-join
      (map (curry reify g) (map triple-end has-child))
      " "
      #:before-first "("
      #:after-last ")")
     (if (not (null? is-call-to))
      (string-join
       (map (curry reify g) (map triple-end (graph-neighborhood-edge-forward G id "has arg")))
       " "
       #:before-first (format "(f~s " (triple-end (car is-call-to)))
       #:after-last ")")
      (begin (display "unable to categorize id:  ") (display id) (newline))))))))

;(reify G 0)

(define (test->graph->file filename)
 (graph->file (string->graph (file->string filename))))

;(test->graph->file "testdata2")

(with
 ((display-on-screen 0 30 WIDTH (- HEIGHT 30) (list 0 'list '())
  child-fun))

 (child-fun (a)
  (with
   ((map
     (compose (curryr get-rep child-fun) triple-end)
     (append
      (graph-neighborhood-edge-forward G (car a) "has child")
      (graph-neighborhood-edge-forward G (car a) "is call to")
      (graph-neighborhood-edge-forward G (car a) "has arg"))))
;   (((compose
;      (curry map (compose (curryr get-rep child-fun) triple-end))
;      (compose
;       (curryr (curry graph-neighborhood-edge-forward G) "has child")
;       car))
;      a))

   (get-rep (id child-fun)
    (with
     ((cons id (get-written id child-fun)))

     (get-written (id child-fun)
      (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
       (if (null? nbhd)
        (let ((nbhd2 (graph-neighborhood-edge-forward G id "is call to")))
         (if (null? nbhd2)
          (let ((nbhd3 (graph-neighborhood-edge-forward G id "has name")))
           (if (null? nbhd3)
            (list 'list '-)
            (list 'named (triple-end (car nbhd3)))))
          (list 'call '--)))
;          (let ((nbhd3 (graph-neighborhood-edge-forward G (triple-end (car nbhd2)) "has name")))
        (list 'terminal (triple-end (car nbhd)))))))))))


;(letrec
; ((id (triple-start (car (graph-edges G))))
;  (child-fun
;   (lambda (a)
;    (let
;     ((get-rep
;       (lambda (id child-fun)
;        (letrec
;         ((get-written
;           (lambda (id child-fun)
;            (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
;             (if (null? nbhd)
;              (get-written (caar (child-fun (cons id '()))) child-fun)
;              (triple-end (car nbhd)))))))
;         (cons id (get-written id child-fun))))))
;     ((compose
;       (curry map (compose (curryr get-rep child-fun) triple-end))
;       (compose
;        (curryr (curry graph-neighborhood-edge-forward G) "has child")
;        car))
;      a)))))
; (display-on-screen 0 30 WIDTH (- HEIGHT 30) (cons id '())
;  child-fun))
;
;(define id (triple-start (car (graph-edges G))))

;(define (root->list root child-fun)
; (if (null? (cdr root))
;  (map (curryr root->list child-fun) (child-fun root))
;  (format "~s" (cdr root))))

;(define (child-fun a)
; (define (get-rep id child-fun)
;  (define (get-written id child-fun)
;   (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
;    (if (null? nbhd)
;     (get-written (caar (child-fun (cons id '()))) child-fun)
;     (triple-end (car nbhd)))))
;  (cons id (get-written id child-fun)))
; ((compose
;   (curry map (compose (curryr get-rep child-fun) triple-end))
;   (compose
;    (curryr (curry graph-neighborhood-edge-forward G) "has child")
;    car))
;  a))

;(display-on-screen 0 30 WIDTH (- HEIGHT 30) (cons id '()) child-fun)

; (display (root->list (cons id '()) child-fun))
; (newline)
