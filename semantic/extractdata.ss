#lang racket

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
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (has-child (graph-neighborhood-edge-forward G parent-id "has child")))
  (with
   ((set! G (graph (graph-vertices G)
                   (if (null? has-child)
                    (if (not-adding-first-arg?)
                     (add-arg-in-middle)
                     (add-arg-at-beginning))
                    (add-child))))
    (update-open)
    (set! Next-id (+ 1 Next-id))
    (update-childfuncs child-fun)
    (go 'right Selected-tree))
 
   (not-adding-first-arg? ()
    (member (triple parent-id "has arg" id) (graph-edges G)))
 
   (add-arg-in-middle ()
    (replace (triple parent-id "has arg" id) (list (triple parent-id "has arg" id) (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'bwahaha)) (graph-edges G)))
 
   (add-arg-at-beginning ()
    (append (list (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'wahaha)) (graph-edges G)))
 
   (add-child ()
    (replace (triple parent-id "has child" id) (list (triple parent-id "has child" id) (triple parent-id "has child" Next-id) (triple Next-id "is written" 'mwahaha)) (graph-edges G)))
 
   (update-open ()
    (set-whole-tree-open! Selected-tree (set-union (list->set (set-map (whole-tree-open Selected-tree) (curry adjust-laddr (selected-id Selected-tree) (last (node-laddr (utterance-node (whole-tree-selection-u Selected-tree)))) (whole-tree-utterance-tree Selected-tree)))) (set (node-laddr (utterance-node (whole-tree-selection-u Selected-tree))))))))))

; error is because adding argument to one call doesn't automatically add it to another call.
(define (adjust-laddr id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (> (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (cons (+ 1 (car laddr)) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))
   (cons (car laddr) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (add-child event)
 (let ((id (selected-id Selected-tree)))
  (with
   ((set! G (graph (graph-vertices G) (add-child-at-beginning)))
    (set! Next-id (+ 1 Next-id))
    (update-childfuncs child-fun)
    (go 'down Selected-tree))
 
   (add-child-at-beginning ()
    (append (list (triple id "has child" Next-id) (triple Next-id "is written" 'kwahaha)) (graph-edges G)))
 
   (update-open ()
    (let ((n (utterance-node (whole-tree-selection-u Selected-tree))))
     (set-whole-tree-open! Selected-tree (set-union (whole-tree-open Selected-tree) (set (node-laddr n) (append (node-laddr n) (list 0))))))))))

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
 (let* ((id (selected-id Selected-tree))
        (is-written (is-written-t id))
        (is-named (graph-neighborhood-edge-forward G id "is named")))
  (with
   ((set! G (graph (graph-vertices G)
                    (if is-written
                     (set-written)
                     (if (null? is-named)
                      (add-name)
                      (set-name)))))
    (update-childfuncs child-fun))

   (get-insert-text ()
    (if (char-numeric? (car (string->list (if (eq? "" INSERTTEXT) "-" INSERTTEXT)))) (string->number INSERTTEXT) (string->symbol (if (eq? "" INSERTTEXT) "-" INSERTTEXT))))

   (set-written ()
    (replace is-written (list (triple id "is written" (get-insert-text))) (graph-edges G)))

   (add-name ()
    (append (graph-edges G) (list (triple id "is named" (get-insert-text)))))

   (set-name ()
    (replace (car is-named) (list (triple id "is named" (get-insert-text))) (graph-edges G))))))

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
(define PARENTLINK1 '())
(define LINK1ADDR '())

(define (add-link event)
 (set! LINK1 (car (node-data (utterance-node (whole-tree-selection-u Selected-tree)))))
 (set! LINK1ADDR (whole-tree-selection Selected-tree))
 (enter-link-mode))

(define (selected-id tree)
 (car (node-data (utterance-node (whole-tree-selection-u tree)))))

(define (selected-parent-id tree)
 (car (node-data (utterance-node (utterance-parent (whole-tree-selection-u tree) tree)))))

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
     (#t '()))))

  (make-scope ()
   (let* ((link2 (selected-id Selected-tree)))
    (with
     ((swap-scope)
      (update-childfuncs child-fun)
      (exit-scope-mode))

     (swap-scope ()
      (let ((has-scope (graph-neighborhood-edge-forward G LINK1 "has scope"))
            (has-env (graph-neighborhood-edge-forward G LINK1 "has env")))
       (if (not (null? has-scope))
        (set! G (graph (graph-vertices G) (replace (car has-scope) (list (triple LINK1 "has scope" link2)) (graph-edges G))))
        (if (not (null? has-env))
         (set! G (graph (graph-vertices G) (replace (car has-env) (list (triple LINK1 "has env" link2)) (graph-edges G))))
         '())))))))))

(define (add-var event)
 (set! LINK1 (selected-id Selected-tree))
 (set! PARENTLINK1 (selected-parent-id Selected-tree))
 (set! LINK1ADDR (whole-tree-selection Selected-tree))
 (enter-var-mode))

(define (handle-var event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((member c '(#\h #\j #\k #\l))
      ((hash-ref key-evs c) event))
     ((eq? c #\return)
      (make-var))
     (#t '()))))

  (make-var ()
   (let* ((link2 (selected-id Selected-tree))
          (parent-link2 (selected-parent-id Selected-tree)))
    (with
     ((if (has-definition?)
       (swap-child)
       (interlocute-and-swap))
      (update-childfuncs child-fun)
      (exit-var-mode))

     (has-definition? ()
      (not (null? (graph-neighborhood-edge-forward G link2 "is defined as"))))

     (swap-child ()
      (set! G (graph (graph-vertices G) (replace (triple PARENTLINK1 "has child" LINK1) (list (triple PARENTLINK1 "has child" link2)) (graph-edges G)))))

     (interlocute-and-swap ()
      (set-whole-tree-selection! Selected-tree LINK1ADDR)
      (set-whole-tree-open! Selected-tree (list->set (set-map (whole-tree-open Selected-tree) (curry adjust-laddr-interlocutor link2 (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree Selected-tree)))))
      (set! G (graph (graph-vertices G) (replace (triple parent-link2 "has child" link2) (list (triple parent-link2 "has child" Next-id) (triple Next-id "is defined as" link2) (triple Next-id "has env" parent-link2)) (graph-edges G))))
      (set! G (graph (graph-vertices G) (replace (triple PARENTLINK1 "has child" LINK1) (list (triple PARENTLINK1 "has child" Next-id)) (graph-edges G))))
      (set! Next-id (+ 1 Next-id))))))))

(define (handle-link event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((member c '(#\h #\j #\k #\l))
      ((hash-ref key-evs c) event))
     ((eq? c #\return)
      (make-link))
     (#t '()))))

  (make-link ()
   (let* ((link2 (selected-id Selected-tree))
          (is-func (is-func-t link2)))
    (with
     ((if is-func
       (add-call-to)
       (let ((has-child (graph-neighborhood-edge-backward G link2 "has child")))
        (if (null? has-child)
         (add-call-to-plus-scope)
         (add-call-to-and-change-original has-child))))
      (update-childfuncs child-fun)
      (exit-link-mode))

     (add-call-to ()
      (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2))))))

     (add-call-to-plus-scope ()
      (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree)))))))

     (add-call-to-and-change-original (has-child)
      (set-whole-tree-selection! Selected-tree LINK1ADDR)
      (set-whole-tree-open! Selected-tree (set-union (set (whole-tree-selection Selected-tree)) (list->set (set-map (whole-tree-open Selected-tree) (curry adjust-laddr-interlocutor link2 (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree Selected-tree))))))
      (set! G (graph (graph-vertices G) (append (foldl (lambda (t res) (replace t (list (triple (triple-start (car has-child) "has arg" (triple-end t)))) res)) (replace (car has-child) (list (triple (triple-start (car has-child)) "has child" Next-id) (triple Next-id "is call to" link2)) (remove (is-written-t LINK1) (graph-edges G))) (cdr has-child)) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree))))))
      (set! Next-id (+ 1 Next-id))))))))

(define (interlocute-lambda event)
 (let ((id (selected-id Selected-tree))
       (parent-id (selected-parent-id Selected-tree)))
  (set-whole-tree-open! Selected-tree (set-union (set (whole-tree-selection Selected-tree)) (list->set (set-map (whole-tree-open Selected-tree) (curry adjust-laddr-interlocutor id (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree Selected-tree))))))
  (set! G (graph (graph-vertices G) (replace (triple parent-id "has child" id) (list (triple parent-id "has child" Next-id) (triple Next-id "is call to" id) (triple id "has scope" parent-id)) (graph-edges G))))
  (set! Next-id (+ 1 Next-id))
  (update-childfuncs child-fun)))

(define (adjust-laddr-interlocutor id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (= (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (begin (display laddr) (newline) (cons (car laddr) (cons 0 (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))))
   (cons (car laddr) (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (delete-link event)
 (let* ((sel (whole-tree-selection-u Selected-tree))
        (id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (child (member (triple parent-id "has child" id) (graph-edges G)))
        (call (member (triple parent-id "is call to" id) (graph-edges G)))
        (arg (member (triple parent-id "has arg" id) (graph-edges G))))
  (with
   ((update-selection)
    (update-open)
    (remove-child)
    (update-childfuncs child-fun))

   (update-selection ()
    (let ((laddr (node-laddr (utterance-node sel))))
     (with
      ((if (find-utterance-from-laddr-safe (whole-tree-utterance-tree Selected-tree) (append (drop-right laddr 1) (list (+ 1 (last laddr)))))
        (select-next-child)
        (if (zero? (last laddr))
         (select-parent)
         (select-previous-child))))

      (select-next-child ()
       '())

      (select-parent ()
       (set-whole-tree-selection! Selected-tree (adjust-laddr-del id (last laddr) (whole-tree-utterance-tree Selected-tree) (drop-right laddr 1))))

      (select-previous-child ()
       (set-whole-tree-selection! Selected-tree (adjust-laddr-del id (last laddr) (whole-tree-utterance-tree Selected-tree) (append (drop-right laddr 1) (list (+ -1 (last laddr))))))))))

   (update-open ()
    (set-whole-tree-open! Selected-tree (list->set (set-map (set-subtract (whole-tree-open Selected-tree) (set-remove (list->set (set-map (whole-tree-open Selected-tree) (curry remove-laddr-del-aux id (last (node-laddr (utterance-node sel))) (whole-tree-utterance-tree Selected-tree)))) '())) (curry adjust-laddr-del id (last (node-laddr (utterance-node sel))) (whole-tree-utterance-tree Selected-tree))))))

   (remove-child ()
    (cond
     (child
      (set! G (graph (graph-vertices G) (remove (car child) (graph-edges G)))))
     (call
      (set! G (graph (graph-vertices G) (remove (car call) (graph-edges G)))))
     (arg
      (set! G (graph (graph-vertices G) (remove (car arg) (graph-edges G)))))
     (#t '()))))))

(define (adjust-laddr-del id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (>= (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (cons (+ -1 (car laddr)) (adjust-laddr-del id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))
   (cons (car laddr) (adjust-laddr-del id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (remove-laddr-del-aux id pos u laddr)
 (remove-laddr-del id pos u laddr laddr))

(define (remove-laddr-del id pos u laddr whole-laddr)
 (if (null? laddr)
  '()
  (begin
;   (display (car laddr)) (display "\t") (display pos) (display "\t") (display id) (display "\t") (display (car (node-data (utterance-node (list-ref (utterance-args u) pos))))) (newline)
  (if (and (= (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (begin (display whole-laddr) (newline) whole-laddr)
   (remove-laddr-del id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr) whole-laddr)))))

(define (reify-code event) (display (reify G 0)) (newline))

(add-key-evs (list #\space add-sibling
                   #\( add-child
                   #\i insert-text
                   #\g add-link
                   #\v add-var
                   #\d delete-link
                   #\r reify-code
                   #\s set-scope
                   #\L interlocute-lambda
                   'insert handle-insert
                   'link handle-link
                   'var handle-var
                   'scope handle-scope))

(define (graph->file g)
 (call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write g f))))

(display (graph->string G))
(if NEWCODE
 (graph->file G)
 '())

(define (reify g id)
 (let ((has-child (graph-neighborhood-edge-forward g id "has child"))
       (is-call-to (graph-neighborhood-edge-forward g id "is call to"))
       (is-written (is-written-t id))
       (has-scope (graph-neighborhood-edge-backward g id "has scope"))
       (is-defined-as (graph-neighborhood-edge-forward g id "is defined as"))
       (has-env (graph-neighborhood-edge-backward g id "has env")))
  (string-append
   (if (null? has-env)
    ""
    (string-append
     "(let ("
     (apply string-append
      (map
       (lambda (t) (format "(v~s ~a)" (triple-start t) (reify g (triple-end (car (graph-neighborhood-edge-forward g (triple-start t) "is defined as"))))))
       has-env))
     ") "))
   (if (null? has-scope)
    ""
    (string-append
     "(letrec ("
     (apply string-append
      (map
       (lambda (t) (format "(f~s (lambda (~a) ~a))" (triple-start t) (string-join (map (compose (curry format "~s") triple-end) (graph-neighborhood-edge-forward g (triple-start t) "has argname")) " ") (reify g (triple-start t))))
       has-scope))
     ") "))
   (if (not (null? has-child))
    (string-join
     (map (curry reify g) (map triple-end has-child))
     " "
     #:before-first "("
     #:after-last ")")
    (if (not (null? is-call-to))
     (string-join
      (map (curry reify g) (map triple-end (graph-neighborhood-edge-forward g id "has arg")))
      " "
      #:before-first (format "(f~s " (triple-end (car is-call-to)))
      #:after-last ")")
     (if (not (null? is-defined-as))
      (format "v~s" id)
      (if is-written
       (format "~s" (triple-end is-written))
       (begin (display "unable to categorize id:  ") (display id) (newline))))))
   (if (null? has-scope)
    ""
    ")")
   (if (null? has-env)
    ""
    ")"))))

;(reify G 0)

(define (test->graph->file filename)
 (graph->file (string->graph (file->string filename))))

;(test->graph->file "testdata2")

(define (yup)
 (display-on-screen 0 330 WIDTH (- HEIGHT 330) (list 0 'list '() '())
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
     (graph-neighborhood-edge-forward G (car a) "is defined as")
     (graph-neighborhood-edge-forward G (car a) "has arg"))))
;  (((compose
;     (curry map (compose (curryr get-rep child-fun) triple-end))
;     (compose
;      (curryr (curry graph-neighborhood-edge-forward G) "has child")
;      car))
;     a))

  (get-rep (id child-fun)
   (with
    ((cons id (get-written)))

    (get-written ()
     (let ((nbhd (is-written-t id)))
      (if nbhd
       (list 'terminal (triple-end nbhd) (nei))
       (let ((nbhd3 (is-func-t id))
             (nbhd4 (is-named-t id)))
        (if nbhd3
         (if nbhd4
          (list 'scoped (triple-end nbhd4) (nei))
          (list 'scoped '--- (nei)))
         (if nbhd4
          (list 'list (triple-end nbhd4) (nei))
          (list 'list '- (nei))))))))

    (nei ()
     (map triple->list (graph-neighborhood-forward G id)))))))

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
