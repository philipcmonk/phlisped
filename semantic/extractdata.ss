#lang racket

; on enter, make selection still visible (generate-utterance-tree ?)
; get next-id from graph
; allow other types to be typed in (string, boolean, symbol, etc)
; deal with argnames

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

(define Next-id 100)

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

(define (add-sibling event)
 (set! G (graph (graph-vertices G)
                (let* ((id (selected-id Selected-tree))
                       (parent-id (selected-parent-id Selected-tree))
                       (nbhd (graph-neighborhood-edge-forward G parent-id "has child")))
                 (if (null? nbhd)
                  (if (member (triple parent-id "has arg" id) (graph-edges G))
                   (replace (triple parent-id "has arg" id) (list (triple parent-id "has arg" id) (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'bwahaha)) (graph-edges G))
                   (append (list (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'wahaha)) (graph-edges G)))
                  (replace (triple parent-id "has child" id) (list (triple parent-id "has child" id) (triple parent-id "has child" Next-id) (triple Next-id "is written" 'mwahaha)) (graph-edges G))))))
 (set! Next-id (+ 1 Next-id))
 (set-whole-tree-open! Selected-tree (set-union (list->set (set-map (whole-tree-open Selected-tree) (curry adjust-laddr (node-laddr (utterance-n (whole-tree-selection Selected-tree)))))) (set (node-laddr (utterance-n (whole-tree-selection Selected-tree))))))
 (update-childfuncs child-fun)
 (go 'right Selected-tree))

(define (adjust-laddr key laddr)
 (if (null? laddr)
  '()
  (if (null? (cdr key))
   (if (<= (car key) (car laddr))
    (cons (+ 1 (car laddr)) (cdr laddr))
    laddr)
   (if (eq? (car key) (car laddr))
    (cons (car laddr) (adjust-laddr (cdr key) (cdr laddr)))
    laddr))))

(define (add-child event)
 (set! G (graph (graph-vertices G)
                (let ((id (selected-id Selected-tree)))
                 (append (list (triple id "has child" Next-id) (triple Next-id "is written" 'kwahaha)) (graph-edges G)))))
 (set! Next-id (+ 1 Next-id))
 (let ((n (utterance-n (whole-tree-selection Selected-tree))))
  (set-whole-tree-open! Selected-tree (set-union (whole-tree-open Selected-tree) (set (node-laddr n) (append (node-laddr n) (list 0))))))
 (update-childfuncs child-fun)
 (go 'down Selected-tree))

(define (insert-text event)
 (set! INSERTTEXT "")
 (enter-insert-mode))

(define INSERTTEXT "")

(define (handle-insert event)
 (let ((c (send event get-key-code)))
  (cond
   ((and (char? c) (not (char-whitespace? c)) (not (char-iso-control? c)) (not (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\))))
    (set! INSERTTEXT (string-append INSERTTEXT (string (send event get-key-code))))
    (paint-info INSERTTEXT #t))
   ((eq? c #\backspace)
    (set! INSERTTEXT (substring INSERTTEXT 0 (- (string-length INSERTTEXT) 1)))
    (paint-info INSERTTEXT #t))
   ((eq? c #\space)
    (write-text-to-graph)
    (exit-insert-mode)
    (add-sibling event)
    (insert-text event))
   ((eq? c #\()
    (add-child event))
   ((eq? c #\))
    (write-text-to-graph)
    (exit-insert-mode)
    (go 'up Selected-tree)
    (insert-text event))
   ((eq? c #\return)
    (write-text-to-graph)
    (exit-insert-mode))
   ((eq? c 'escape)
    (exit-insert-mode))
   (#t '()))))

(define (write-text-to-graph)
 (set! G (graph (graph-vertices G)
                (let* ((id (selected-id Selected-tree))
                       (nbhd (is-written-t id))
                       (nbhd2 (graph-neighborhood-edge-forward G id "is named"))
                       (res (if (char-numeric? (car (string->list (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))) (string->number INSERTTEXT) (string->symbol (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))))
                 (if nbhd
                  (replace nbhd (list (triple id "is written" res)) (graph-edges G))
                  (if (null? nbhd2)
                   (append (graph-edges G) (list (triple id "is named" res)))
                   (replace (car nbhd2) (list (triple id "is named" res)) (graph-edges G)))))))
 (update-childfuncs child-fun))

(define (replace t1 t2s es)
 (append (takef es (negate (curry equal? t1))) t2s (cdr (member t1 es))))

(define (is-written-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is written")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define (is-func-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "has scope")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define LINK1 '())

(define (add-link event)
 (set! LINK1 (car (node-data (utterance-n (whole-tree-selection Selected-tree)))))
 (enter-link-mode))

(define (selected-id tree)
 (car (node-data (utterance-n (whole-tree-selection tree)))))

(define (selected-parent-id tree)
 (car (node-data (utterance-n (utterance-parent (whole-tree-selection tree) tree)))))

(define (handle-link event)
 (let ((c (send event get-key-code)))
  (cond
   ((member c '(#\h #\j #\k #\l))
    ((hash-ref key-evs c) event))
   ((eq? c #\return)
    (let* ((link2 (selected-id Selected-tree))
           (nbhd (is-func-t link2)))
     (if nbhd
      (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2)))))
      (let ((nbhd2 (graph-neighborhood-edge-backward G link2 "has child")))
       (if (null? nbhd2)
        (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree))))))
        (begin
         (set! G (graph (graph-vertices G) (append (replace (car nbhd2) (list (triple (triple-start (car nbhd2)) "has child" Next-id) (triple Next-id "is call to" link2)) (remove (is-written-t LINK1) (graph-edges G))) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree))))))
         (set! Next-id (+ 1 Next-id)))))))
    (update-childfuncs child-fun)
    (exit-link-mode))
   (#t '()))))

(define (delete-link event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (child (member (triple parent-id "has child" id) (graph-edges G)))
        (call (member (triple parent-id "is call to" id) (graph-edges G)))
        (arg (member (triple parent-id "has arg" id) (graph-edges G))))
  (cond
   (child
    (set! G (graph (graph-vertices G) (remove (car child) (graph-edges G)))))
   (call
    (set! G (graph (graph-vertices G) (remove (car call) (graph-edges G)))))
   (arg
    (set! G (graph (graph-vertices G) (remove (car arg) (graph-edges G)))))
   (#t '())))
 (update-childfuncs child-fun))

(define (reify-code event) (display (reify G 0)) (newline))

(add-key-evs (list #\space add-sibling
                   #\( add-child
                   #\i insert-text
                   #\g add-link
                   #\d delete-link
                   #\r reify-code
                   'insert handle-insert
                   'link handle-link))

(define (graph->file g)
 (call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write g f))))

(display (graph->string G))
(if NEWCODE
 (graph->file G)
 '())

(define (reify g id)
 (let ((has-child (graph-neighborhood-edge-forward G id "has child"))
       (is-call-to (graph-neighborhood-edge-forward G id "is call to"))
       (is-written (is-written-t id))
       (has-scope (graph-neighborhood-edge-backward G id "has scope")))
  (string-append
   (if (null? has-scope)
    ""
    (apply string-append
     (map
      (lambda (t) (format "(define (f~s ~a) ~a)" (triple-start t) (string-join (map (compose (curry format "~s") triple-end) (graph-neighborhood-edge-forward G (triple-start t) "has argname")) " ") (reify g (triple-start t))))
      has-scope)))
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
     (if is-written
      (format "~s" (triple-end is-written))
      (begin (display "unable to categorize id:  ") (display id) (newline))))))))

;(reify G 0)

(define (test->graph->file filename)
 (graph->file (string->graph (file->string filename))))

;(test->graph->file "testdata2")

(define (yup)
 (display-on-screen 0 30 WIDTH (- HEIGHT 30) (list 0 'list '())
  child-fun))
 
(define (is-call-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is call to")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define (is-named-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is named")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define (child-fun a)
 (with
  ((map
    (compose (curryr get-rep child-fun) triple-end)
    (append
     (graph-neighborhood-edge-forward G (car a) "has child")
     (graph-neighborhood-edge-forward G (car a) "is call to")
     (graph-neighborhood-edge-forward G (car a) "has arg"))))
;  (((compose
;     (curry map (compose (curryr get-rep child-fun) triple-end))
;     (compose
;      (curryr (curry graph-neighborhood-edge-forward G) "has child")
;      car))
;     a))

  (get-rep (id child-fun)
   (with
    ((cons id (get-written id child-fun)))

    (get-written (id child-fun)
     (let ((nbhd (is-written-t id)))
      (if nbhd
       (list 'terminal (triple-end nbhd))
;       (let ((nbhd2 (is-call-t id)))
;        (if nbhd2
;         (list 'call '--)
         (let ((nbhd3 (is-func-t id)))
          (if nbhd3
           (let ((nbhd4 (is-named-t id)))
            (if nbhd4
             (list 'scoped (triple-end nbhd4))
             (list 'scoped '---)))
           (list 'list '-))))))))))
;         (let ((nbhd3 (graph-neighborhood-edge-forward G (triple-end (car nbhd2)) "is named")))

(yup)


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
