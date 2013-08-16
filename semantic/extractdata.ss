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

(set! Next-id (triple-end (car (graph-neighborhood-edge-forward G 'next-id "is"))))

; XXX shouldn't allow adding args
(define (add-sibling event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (is-call-to (graph-neighborhood-edge-forward G parent-id "is call to")))
  (with
   ((if (not (null? is-call-to))
     '()
     (begin
      (undo-push)
      (set! G (graph (graph-vertices G) (add-child)))
;                      (if (adding-first-arg?)
;                       (add-arg-at-beginning)
;                       (add-arg-in-middle)))))
      (update-open)
      (set! Next-id (+ 1 Next-id))
      (update-childfuncs child-fun)
      (go 'right Selected-tree))))
 
   (add-child ()
    (replace (triple parent-id "has child" id) (list (triple parent-id "has child" id) (triple parent-id "has child" Next-id) (triple Next-id "is written" 'mwahaha)) (graph-edges G)))
 
;   (adding-first-arg? ()
;    (member (triple parent-id "is call to" id) (graph-edges G)))
; 
;   (add-arg-at-beginning ()
;    (append (list (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'wahaha)) (graph-edges G)))
;
;   (add-arg-in-middle ()
;    (replace (triple parent-id "has arg" id) (list (triple parent-id "has arg" id) (triple parent-id "has arg" Next-id) (triple Next-id "is written" 'umsoyeah)) (graph-edges G)))
 
   (update-open ()
    (for-all-trees
     (lambda (tree)
      (set-whole-tree-open! tree
                            (set-union
                             (list->set
                              (set-map
                               (whole-tree-open tree)
                               (curry adjust-laddr id
                                      (last (whole-tree-selection Selected-tree))
                                      (whole-tree-utterance-tree tree))))
                             (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree)) ))))))))))))

; error is because adding argument to one call doesn't automatically add it to another call.
(define (adjust-laddr id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (> (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (cons (+ 1 (car laddr)) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))
   (cons (car laddr) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (add-child event)
 (let* ((id (selected-id Selected-tree))
        (is-call-to (graph-neighborhood-edge-forward G id "is call to"))
        (is-defined-as (graph-neighborhood-edge-forward G id "is defined as")))
  (with
   ((if (not (and (null? is-call-to) (null? is-defined-as)))
     '()
     (begin
      (undo-push)
      (set! G (graph (graph-vertices G) (add-child-at-beginning)))
      (set! Next-id (+ 1 Next-id))
      (update-open)
      (update-childfuncs child-fun)
      (go 'down Selected-tree))))
   
     (add-child-at-beginning ()
      (append (list (triple id "has child" Next-id) (triple Next-id "is written" 'kwahaha)) (graph-edges G)))
   
     (update-open ()
      (for-all-trees
       (lambda (tree)
        (set-whole-tree-open! tree (set-union (whole-tree-open tree) (set (whole-tree-selection Selected-tree) (append (whole-tree-selection Selected-tree) (list 0)))))))))))

(define INSERTTEXT "")

(define (insert-text event)
 (set! INSERTTEXT "")
 (enter-insert-mode))

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
   ((undo-push)
    (set! G (graph (graph-vertices G)
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
(define LINK1PARENT '())
(define LINK1ADDR '())

(define (add-link event)
 (set! LINK1 (car (node-data (utterance-node (whole-tree-selection-u Selected-tree)))))
 (set! LINK1ADDR (whole-tree-selection Selected-tree))
 (enter-link-mode))

(define (selected-id tree)
 (car (node-data (utterance-node (whole-tree-selection-u tree)))))

(define (selected-parent-id tree)
 (car (node-data (utterance-node (utterance-parent (whole-tree-selection-u tree) tree)))))

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
          (has-child (member (triple parent-link2 "has child" link2) (graph-edges G)))
          (has-arg (member (triple parent-link2 "has arg" link2) (graph-edges G)))
          (parent-link2-t (if has-child (car has-child) (if has-arg (car has-arg) '())))
;          (hijito (triple-end (car (graph-neighborhood-edge-forward G LINK1 "is call to"))))
          (is-defined-as (graph-neighborhood-edge-forward G link2 "is defined as"))
          (arg-id Next-id))
    (with
     ((undo-push)
      (set! Next-id (+ 1 Next-id))
      (add-arg-to-hijito)
      (add-arg-to-call)
      (convert-var-to-arg)
      (update-childfuncs child-fun)
      (exit-argify-mode))

     (add-arg-to-hijito ()
      (set! G (graph (graph-vertices G) (append (graph-edges G) (list (triple LINK1 "has formal arg" arg-id) (triple arg-id "is written" 'arger) (triple arg-id "is reified as" (string->symbol (format "a~s" arg-id))))))))

     (add-arg-to-call ()
      (let ((replacement (if (null? is-defined-as) link2 (triple-end (car is-defined-as)))))
       (set! G (graph (graph-vertices G) (append (graph-edges G) (list (triple LINK1PARENT "has arg" replacement))))))
      (set! G (graph (graph-vertices G) (append (graph-edges G) (flatten (map (lambda (t) (set! Next-id (+ 1 Next-id)) (list (triple (triple-start t) "has arg" (- Next-id 1)) (triple (- Next-id 1) "is written" 'yo)))  (remove (triple LINK1PARENT "is call to" LINK1) (graph-neighborhood-edge-backward G LINK1 "is call to"))))))))

     (convert-var-to-arg ()
      (set! G (graph (graph-vertices G) ((if (null? is-defined-as) swap-normal swap-var) LINK1 (graph-edges G)))))

     (swap-normal (in-id res)
      (replace parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) arg-id)) (graph-edges G)))
;      (foldl
;       swap-normal
;       (foldl
;        (lambda (t res)
;         (if (eq? (triple-end t) link2)
;          (replace t (list (triple in-id "has child" Next-id)) res)
;          res))
;        res
;        (graph-neighborhood-edge-forward G in-id "has child"))
;       (map triple-end (graph-neighborhood-edge-forward G in-id "has child"))))

     (swap-var (in-id res)
      (replace (car is-defined-as) (list (triple link2 "is defined as" arg-id)) (graph-edges G))))))))

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
    (with
     ((undo-push)
      (swap-scope)
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
 (set! LINK1PARENT (selected-parent-id Selected-tree))
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
     ((eq? c 'escape)
      (exit-var-mode))
     (#t '()))))

  (make-var ()
   (let* ((link2 (selected-id Selected-tree))
          (parent-link2 (selected-parent-id Selected-tree))
          (has-child1 (member (triple LINK1PARENT "has child" LINK1) (graph-edges G)))
          (has-arg1 (member (triple LINK1PARENT "has arg" LINK1) (graph-edges G)))
          (parent-link1-t (if has-child1 (car has-child1) (if has-arg1 (car has-arg1) '())))
          (has-child2 (member (triple parent-link2 "has child" link2) (graph-edges G)))
          (has-arg2 (member (triple parent-link2 "has arg" link2) (graph-edges G)))
          (parent-link2-t (if has-child2 (car has-child2) (if has-arg2 (car has-arg2) '()))))
    (with
     ((undo-push)
      (if (has-definition?)
       (swap-child)
       (interlocute-and-swap))
      (update-childfuncs child-fun)
      (exit-var-mode))

     (has-definition? ()
      (not (null? (graph-neighborhood-edge-forward G link2 "is defined as"))))

     (swap-child ()
      (set! G (graph (graph-vertices G) (replace parent-link2-t (list (triple LINK1PARENT (triple-edge parent-link1-t) link2)) (graph-edges G))))
      (let ((has-env (car (graph-neighborhood-edge-forward G link2 "has env"))))
       (display has-env) (newline)
       (set! G (graph (graph-vertices G) (replace has-env (list (triple link2 "has env" (common-ancestor link2 (triple-end has-env)))) (graph-edges G))))))

     (interlocute-and-swap ()
      (for-all-trees
       (lambda (tree)
        (set-whole-tree-selection! tree (adjust-laddr-interlocutor link2 (last (whole-tree-selection tree)) (whole-tree-utterance-tree tree)))
        (set-whole-tree-open! tree (adjust-laddr-interlocute link2 (last (whole-tree-selection Selected-tree)) tree))))
      (set-whole-tree-selection! Selected-tree LINK1ADDR)
      (set! G (graph (graph-vertices G) (replace parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) Next-id) (triple Next-id "is defined as" link2) (triple Next-id "has env" (common-ancestor LINK1 link2))) (graph-edges G))))
      (set! G (graph (graph-vertices G) (replace parent-link1-t (list (triple LINK1PARENT (triple-edge parent-link1-t) Next-id)) (graph-edges G))))
      (set! Next-id (+ 1 Next-id))))))))

(define (common-ancestor id1 id2)
 (with
  ((find-first-overlap (ancestors id1 (list id1)) (ancestors id2 (list id2))))

  ; XXX should also cross "has env", right?
  (ancestors (id l)
   (let* ((has-child (graph-neighborhood-edge-backward G id "has child"))
          (has-arg (graph-neighborhood-edge-backward G id "has arg"))
          (has-parent (if (null? has-child) has-arg has-child)))
    (if (null? has-parent)
     l
     (ancestors (triple-start (car has-child)) (append l (list (triple-start (car has-child))))))))

  (find-first-overlap (l1 l2)
   (if (null? l1)
    #f
    (if (member (car l1) l2)
     (car l1)
     (find-first-overlap (cdr l1) l2))))))

(define (handle-link event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((member c '(#\h #\j #\k #\l))
      ((hash-ref key-evs c) event))
     ((eq? c #\return)
      (make-link))
     ((eq? c 'escape)
      (exit-link-mode))
     (#t '()))))

  (make-link ()
   (let* ((link2 (selected-id Selected-tree))
          (is-func (is-func-t link2)))
    (with
     ((undo-push)
      (if is-func
       (add-call-to)
;       (let ((has-child (graph-neighborhood-edge-backward G link2 "has child")))
;        (if (null? has-child)
         (add-call-to-plus-scope))
;         (add-call-to-and-change-original has-child))))
      (update-childfuncs child-fun)
      (exit-link-mode))

     (add-call-to ()
      (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2)) (flatten (map (lambda (t) (set! Next-id (+ 1 Next-id)) (list (triple LINK1 "has arg" (- Next-id 1)) (triple (- Next-id 1) "is written" 'bill))) (graph-neighborhood-edge-forward G link2 "has formal arg")))))))

     (add-call-to-plus-scope ()
      (set! G (graph (graph-vertices G) (append (remove (is-written-t LINK1) (graph-edges G)) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree))))))))))))

;     (add-call-to-and-change-original (has-child)
;      (for-all-trees
;       (lambda (tree)
;        (set-whole-tree-selection! tree (adjust-laddr-interlocutor link2 (last (whole-tree-selection tree)) (whole-tree-utterance-tree tree)))
;        (set-whole-tree-open! tree (adjust-laddr-interlocute link2 (last (whole-tree-selection Selected-tree)) tree))))
;      (set-whole-tree-selection! Selected-tree LINK1ADDR)
;      (set! G (graph (graph-vertices G) (append (replace (car has-child) (list (triple (triple-start (car has-child)) "has child" Next-id) (triple Next-id "is call to" link2)) (remove (is-written-t LINK1) (graph-edges G))) (cdr has-child) (list (triple LINK1 "is call to" link2) (triple link2 "has scope" (selected-parent-id Selected-tree))))))
;      (set! Next-id (+ 1 Next-id))))))))

(define (interlocute-lambda event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (has-child (member (triple parent-id "has child" id) (graph-edges G)))
        (has-arg (member (triple parent-id "has arg" id) (graph-edges G)))
        (parent-link-t (if has-child (car has-child) (if has-arg (car has-arg) '()))))
  (undo-push)
  (for-all-trees
   (lambda (tree)
    (set-whole-tree-open! tree (adjust-laddr-interlocute id (last (whole-tree-selection Selected-tree)) tree))))
  (set! G (graph (graph-vertices G) (replace parent-link-t (list (triple parent-id (triple-edge parent-link-t) Next-id) (triple Next-id "is call to" id) (triple id "has scope" parent-id)) (graph-edges G))))
  (set! Next-id (+ 1 Next-id))
  (update-childfuncs child-fun)))

(define (adjust-laddr-interlocute id pos tree)
 (with
  ((set-union
    (adjust-currently-open)
    (add-in-all-nodes-with-id)))

  (adjust-currently-open ()
   (list->set (set-map (whole-tree-open tree) (curry adjust-laddr-interlocutor id pos (whole-tree-utterance-tree tree)))))

  (add-in-all-nodes-with-id ()
   (set-remove (list->set (set-map (whole-tree-open tree) (lambda (laddr) (if (eq? id (car (node-data (utterance-node (find-utterance-from-laddr-safe (whole-tree-utterance-tree tree) laddr))))) laddr '_)))) '_))))

(define (adjust-laddr-interlocutor id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (= (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (begin (display laddr) (newline) (cons (car laddr) (cons 0 (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))))
   (cons (car laddr) (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (delete-link event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (child (member (triple parent-id "has child" id) (graph-edges G)))
        (call (member (triple parent-id "is call to" id) (graph-edges G))))
  (with
   ((if (not child)
     '()
     (begin
      (undo-push)
      (update-open)
      (update-selection)
      (remove-child)
      (update-childfuncs child-fun))))

   (update-selection ()
    (let ((laddr (whole-tree-selection Selected-tree)))
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
    (for-all-trees
     (lambda (tree)
      (with
       ((set-whole-tree-open! tree (list->set (set-map (remove-deleted-laddrs) (adjust-laddrs)))))

       (remove-deleted-laddrs ()
        (set-subtract
         (whole-tree-open tree)
          (set-remove (list->set (set-map (whole-tree-open tree) (curry remove-laddr-del-aux id (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree tree)))) '())))
 
       (adjust-laddrs ()
        (curry adjust-laddr-del id (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree tree)))))))

   (remove-child ()
    (cond
     (child
      (set! G (graph (graph-vertices G) (remove (car child) (graph-edges G)))))
     (call
      (set! G (graph (graph-vertices G) (remove (car call) (graph-edges G)))))
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
  (if (and (= (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (begin (display whole-laddr) (newline) whole-laddr)
   (remove-laddr-del id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr) whole-laddr)))))

(define (reify-code event) (display (reify G 0)) (newline))

(define (write-to-file event)
 (graph->file G))

(define UNDOSTACK '())
(define REDOSTACK '())

(define (undo-push)
 (set! REDOSTACK '())
 (set! UNDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) UNDOSTACK)))

(define (undo-pop event)
 (if (null? UNDOSTACK)
  '()
  (begin
   (set! REDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) REDOSTACK))
   (set! G (caar UNDOSTACK))
   (set-whole-tree-open! Selected-tree (cadar UNDOSTACK))
   (set-whole-tree-selection! Selected-tree (caddar UNDOSTACK))
   (set! UNDOSTACK (cdr UNDOSTACK))
   (update-childfuncs child-fun))))

(define (redo-pop event)
 (if (null? REDOSTACK)
  '()
  (begin
   (set! UNDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) UNDOSTACK))
   (set! G (caar REDOSTACK))
   (set-whole-tree-open! Selected-tree (cadar REDOSTACK))
   (set-whole-tree-selection! Selected-tree (caddar REDOSTACK))
   (set! REDOSTACK (cdr REDOSTACK))
   (update-childfuncs child-fun))))

(define Search-text "")
(define Search-tree '())

(define (search event)
 (set! Search-text "")
 (enter-search-mode))

(define (handle-search event)
 (let ((c (send event get-key-code)))
  (cond
   ((and (char? c) (not (char-whitespace? c)) (not (char-iso-control? c)) (not (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\))))
    (set! Search-text (string-append Search-text (string (send event get-key-code))))
    (set-search-results (map triple->list (search-text Search-text)))
    (show-search-tree get-rep)
    (paint-info Search-text #t)
    (send Thecanvas on-paint))
   ((eq? c #\backspace)
    (set! Search-text (substring Search-text 0 (- (string-length Search-text) 1)))
    (paint-info Search-text #t))
   ((eq? c #\return)
    (exit-search-mode))
   ((eq? c 'escape)
    (remove-search-tree)
    (send Thecanvas on-paint)
    (exit-search-mode))
   (#t '()))))

(define (search-text text)
 (let ((regex (regexp text))
       (texts (graph-filter G (lambda (t) (or (equal? (triple-edge t) "is written") (equal? (triple-edge t) "is named"))))))
  (filter (lambda (t) (regexp-match? regex (format "~a" (triple-end t)))) texts)))


; (display text) (display "\t") (display id) (newline)
; (let ((is-written (is-written-t id))
;       (is-named (is-named-t id))
;       (regex (regexp Search-text)))
;  (apply append
;   (if (and is-written (regexp-match? regex (format "~a" (triple-end is-written))))
;    (list is-written)
;    '())
;   (if (and is-named (regexp-match? regex (format "~a" (triple-end is-named))))
;    (list is-named)
;    '())
;   (map (compose (curry search-text text) triple-end) (append (graph-neighborhood-edge-forward G id "has child") (graph-neighborhood-edge-forward G id "is call to") (graph-neighborhood-edge-forward G id "has arg") (graph-neighborhood-edge-forward G id "is defined as"))))))

(add-key-evs (list #\space add-sibling
                   #\( add-child
                   #\i insert-text
                   #\g add-link
                   #\v add-var
                   #\d delete-link
                   #\r reify-code
                   #\s set-scope
                   #\L interlocute-lambda
                   #\a argify
                   #\u undo-pop
                   #\r redo-pop
                   #\/ search
                   'f2 write-to-file
                   'insert handle-insert
                   'link handle-link
                   'var handle-var
                   'scope handle-scope
                   'argify handle-argify
                   'search handle-search))

(define (graph->file g)
 (let ((g (graph (graph-vertices g) (replace (car (graph-neighborhood-edge-forward g 'next-id "is")) (list (triple 'next-id "is" Next-id)) (graph-edges g)))))
  (call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write g f)))))

(display (graph->string G))
(if NEWCODE
 (graph->file G)
 '())

(define (reify g id)
 (let ((has-child (graph-neighborhood-edge-forward g id "has child"))
       (is-call-to (graph-neighborhood-edge-forward g id "is call to"))
       (has-arg (graph-neighborhood-edge-forward g id "has arg"))
       (is-written (is-written-t id))
       (has-scope (graph-neighborhood-edge-backward g id "has scope"))
       (is-defined-as (graph-neighborhood-edge-forward g id "is defined as"))
       (has-env (graph-neighborhood-edge-backward g id "has env"))
       (is-reified-as (graph-neighborhood-edge-forward g id "is reified as")))
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
       (lambda (t) (format "(f~s (lambda (~a) ~a))" (triple-start t) (string-join (map (compose (curry format "a~s") triple-end) (graph-neighborhood-edge-forward g (triple-start t) "has formal arg")) " ") (reify g (triple-start t))))
       has-scope))
     ") "))
   (if (not (null? is-call-to))
    (string-join
     (map (curry reify g) (map triple-end has-arg))
     " "
     #:before-first (format "(f~s " (triple-end (car is-call-to)))
     #:after-last ")")
    (if (not (null? has-child))
     (string-join
      (map (curry reify g) (map triple-end has-child))
      " "
      #:before-first "("
      #:after-last ")")
     (if (not (null? is-defined-as))
      (format "v~s" id)
      (if (not (null? is-reified-as))
       (format "~s" (triple-end (car is-reified-as)))
       (if is-written
        (format "~s" (triple-end is-written))
        (begin (display "unable to categorize id:  ") (display id) (newline)))))))
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
 (display-on-screen 0 330 WIDTH (- HEIGHT 330) (list 0 'list '() '() '()) child-fun)
 (display "um, so yeah\n"))
 
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
 (map
  (compose get-rep triple-end)
  (append
   (graph-neighborhood-edge-forward G (car a) "is call to")
   (graph-neighborhood-edge-forward G (car a) "has child")
   (graph-neighborhood-edge-forward G (car a) "has arg")
   (graph-neighborhood-edge-forward G (car a) "is defined as"))))
;  (((compose
;     (curry map (compose (curryr get-rep child-fun) triple-end))
;     (compose
;      (curryr (curry graph-neighborhood-edge-forward G) "has child")
;      car))
;     a))

(define free-variables (hash))

(define (update-free-variables)
 (with
  ((for-each
    update-free-variable-id
    (remove-duplicates (map triple-start (graph-edges G)))))

  (update-free-variable-id (id)
   (if (hash-has-key? free-variables id)
    '()
    (let ((free-from-children (remove-duplicates
                               (flatten
                                (append
                                 (map
                                  (lambda (child) (update-free-variable-id child) (hash-ref free-variables child))
                                  (append
                                   (map triple-end (append (graph-neighborhood-edge-forward G id "has child") (graph-neighborhood-edge-forward G id "has arg")))
                                   (map triple-start (append (graph-neighborhood-edge-backward G id "has scope") (graph-neighborhood-edge-backward G id "has env")))))
                                 (map triple-end (append (graph-neighborhood-edge-forward G id "is call to")))
                                 (map triple-start (append (graph-neighborhood-edge-forward G id "is defined as")))
                                 (if (null? (graph-neighborhood-edge-forward G id "is reified as")) '() (list id))))))
          (defined-here (append
                         (map triple-start (append (graph-neighborhood-edge-backward G id "has scope") (graph-neighborhood-edge-backward G id "has env")))
                         (map triple-end (graph-neighborhood-edge-forward G id "has formal arg")))))
     (display id) (display "\n\t") (display free-from-children) (display "\n\t\t") (display defined-here) (display "\n\t\t\t") (display (set->list (set-subtract (list->set free-from-children) (list->set defined-here)))) (newline)
     (set! free-variables (hash-set free-variables id (set->list (set-subtract (list->set free-from-children) (list->set defined-here))))))))))

(update-free-variables)

(define (get-rep id)
 (with
  ((append (list id) (get-written) (list (nei) (get-free-variables))))

  (get-written ()
   (let ((is-func (is-func-t id))
         (is-named (is-named-t id)))
    (if is-func
     (if is-named
      (list 'scoped (triple-end is-named))
      (list 'scoped '---))
     (let ((has-child (graph-neighborhood-edge-forward G id "has child"))
           (is-written (is-written-t id))
           (is-defined-as (graph-neighborhood-edge-forward G id "is defined as")))
      (if (not (null? has-child))
       (if is-named
        (list 'list (triple-end is-named))
        (if is-written
         (list 'list (triple-end is-written))
         (list 'list '-)))
       (if is-written
        (list 'terminal (triple-end is-written))
        (if (not (null? is-defined-as))
         (if is-named
          (list 'var (triple-end is-named))
          (list 'var '--))
         (list 'unknown 'unknown))))))))

  (nei ()
   (map triple->list (append (graph-neighborhood-forward G id) (graph-neighborhood-backward G id))))

  (get-free-variables ()
   (hash-ref free-variables id))))

(display "wait, what in the world?\n")
(for-all-trees (lambda (tree) (display "asdfghjkl;\n") (display tree) (newline)))
(yup)

(for-all-trees (lambda (tree) (display "qwertyuiop\n") (display tree) (newline)))

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
