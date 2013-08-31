#lang racket

(require "graph.ss" "find.ss" "disp.ss")
(require racket/set)

(provide Thecanvas Info (all-defined-out))

(define FILENAME "extractdata.ss")
(define GRFILE "datagetcolor")

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

(define (add-sibling event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree)))
  (with
   ((undo-push)
 (display "yo\n")
    (set! G (graph (graph-vertices G) (add-child)))
 (display "yo\n")
    (update-open)
    (set! Next-id (+ 1 Next-id))
    (update-data)
    (update-childfuncs child-fun)
    (go 'right Selected-tree))
 
   (add-child ()
    (replace (triple parent-id "has child" id) (list (triple parent-id "has child" id) (triple parent-id "has child" Next-id) (triple Next-id "is written" '-)) (graph-edges G)))
 
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
                             (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree))))))))))))))

(define (adjust-laddr id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (> (car laddr) pos) (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (cons (+ 1 (car laddr)) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))
   (cons (car laddr) (adjust-laddr id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

(define (add-child event)
 (let* ((id (selected-id Selected-tree))
        (is-defined-as (graph-neighborhood-edge-forward G id "is defined as")))
  (with
   ((if (not (null? is-defined-as))
     '()
     (begin
      (undo-push)
      (set! G (graph (graph-vertices G) (add-child-at-beginning)))
      (set! Next-id (+ 1 Next-id))
      (update-open)
      (update-data)
      (update-childfuncs child-fun)
      (go 'down Selected-tree))))
   
     (add-child-at-beginning ()
      (append (list (triple id "has child" Next-id) (triple Next-id "is written" '-)) (graph-edges G)))
   
     (update-open ()
      (for-all-trees
       (lambda (tree)
        (set-whole-tree-open! tree (set-union (whole-tree-open tree) (set (whole-tree-selection Selected-tree) (append (whole-tree-selection Selected-tree) (list 0)))))))))))

(define INSERTTEXT "")

(define (insert-text event)
 (set! INSERTTEXT "")
 (enter-insert-mode))

(define (handle-insert event)
 (with
  ((let ((c (send event get-key-code)))
    (cond
     ((and (eq? c #\n) (send event get-control-down))
      (scroll-search-results)
      (show-search-tree get-rep)
      (send Thecanvas on-paint))
     ((and (char? c) (not (char-whitespace? c)) (not (char-iso-control? c)) (not (member c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\))))
      (set! INSERTTEXT (string-append INSERTTEXT (string (send event get-key-code))))
      (show-results))
     ((eq? c #\backspace)
      (set! INSERTTEXT (substring INSERTTEXT 0 (- (string-length INSERTTEXT) 1)))
      (show-results))
     ((eq? c #\space)
      (remove-search-tree)
      (if (and (send event get-shift-down) (not (null? Search-results)))
       (link-to (selected-id Selected-tree) (caar Search-results))
       (write-text-to-graph))
      (exit-insert-mode)
      (add-sibling event)
      (insert-text event))
     ((eq? c #\()
      (remove-search-tree)
      (write-text-to-graph)
      (exit-insert-mode)
      (add-child event)
      (insert-text event))
     ((eq? c #\))
      (remove-search-tree)
      (write-text-to-graph)
      (exit-insert-mode)
      (go 'up Selected-tree)
      (insert-text event))
     ((eq? c #\return)
      (remove-search-tree)
      (if (and (send event get-shift-down) (not (null? Search-results)))
       (link-to (selected-id Selected-tree) (caar Search-results))
       (write-text-to-graph))
      (exit-insert-mode))
     ((eq? c 'escape)
      (remove-search-tree)
      (send Thecanvas on-paint)
      (exit-insert-mode))
     (#t '()))))

  (show-results ()
   (set-search-results (search-bound-variables (node-data (utterance-node (whole-tree-selection-u Selected-tree))) INSERTTEXT))
   (show-search-tree get-rep)
   (set-info INSERTTEXT)
   (send Thecanvas on-paint))))

(define (link-to id1 id2)
 (let* ((is-defined-as-2 (graph-neighborhood-edge-forward G id2 "is defined as"))
        (has-child (graph-neighborhood-edge-backward G id1 "has child"))
        (is-defined-as-1 (graph-neighborhood-edge-backward G id1 "is defined as"))
        (parent-id1-t (if (not (null? has-child)) (car has-child) (if (not (null? is-defined-as-1)) (car is-defined-as-1) '()))))
  (cond
   ((not (null? is-defined-as-2))
    (set! G (graph (graph-vertices G) (replace parent-id1-t (list (triple (triple-start parent-id1-t) (triple-edge parent-id1-t) id2)) (graph-edges G)))))
   (#t '())))
 (update-data)
 (update-childfuncs child-fun))

(define (search-bound-variables data text)
 (let ((regex (regexp text))
       (texts (cadddr (cddr data))))
  (filter (lambda (t) (regexp-match? regex (format "~a" (cadr t)))) texts)))

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
    (update-data)
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
 (let ((nbhd (graph-neighborhood-edge-forward G id "is function")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

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
          (parent-link2-t (if has-child (car has-child) '()))
          (is-defined-as (graph-neighborhood-edge-forward G link2 "is defined as"))
          (arg-id Next-id))
    (with
     ((undo-push)
      (set! Next-id (+ 1 Next-id))
      (add-arg-to-hijito)
      (add-arg-to-call)
      (convert-var-to-arg)
      (update-data)
      (update-childfuncs child-fun)
      (exit-argify-mode))

     (add-arg-to-hijito ()
      (set! G (graph (graph-vertices G) (append (graph-edges G) (list (triple LINK1 "has formal arg" arg-id) (triple arg-id "is written" 'arger) (triple arg-id "is reified as" (string->symbol (format "a~s" arg-id))))))))

     (add-arg-to-call ()
      (let ((replacement (if (null? is-defined-as) link2 (triple-end (car is-defined-as)))))
       (set! G (graph (graph-vertices G) (append (graph-edges G) (list (triple LINK1PARENT "has child" replacement)))))))

     (convert-var-to-arg ()
      (set! G (graph (graph-vertices G) ((if (null? is-defined-as) swap-normal swap-var) LINK1 (graph-edges G)))))

     (swap-normal (in-id res)
      (replace parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) arg-id)) (graph-edges G)))

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
      (update-data)
      (update-childfuncs child-fun)
      (exit-scope-mode))

     (swap-scope ()
      (let ((has-env (graph-neighborhood-edge-forward G LINK1 "has env")))
       (if (not (null? has-env))
        (set! G (graph (graph-vertices G) (replace (car has-env) (list (triple LINK1 "has env" link2)) (graph-edges G))))
        '()))))))))

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
          (parent-link1-t (if has-child1 (car has-child1) '()))
          (has-child2 (member (triple parent-link2 "has child" link2) (graph-edges G)))
          (parent-link2-t (if has-child2 (car has-child2) '())))
    (with
     ((undo-push)
      (if (has-definition?)
       (swap-child)
       (interlocute-and-swap))
      (update-data)
      (update-childfuncs child-fun)
      (exit-var-mode))

     (has-definition? ()
      (not (null? (graph-neighborhood-edge-forward G link2 "is defined as"))))

     (swap-child ()
      (set! G (graph (graph-vertices G) (replace parent-link1-t (list (triple LINK1PARENT (triple-edge parent-link1-t) link2)) (graph-edges G))))
      (let ((has-env (car (graph-neighborhood-edge-forward G link2 "has env"))))
       (display has-env) (newline)
       (set! G (graph (graph-vertices G) (replace has-env (list (triple link2 "has env" (common-ancestor link2 (triple-end has-env)))) (graph-edges G))))))

     (interlocute-and-swap ()
      (for-all-trees
       (lambda (tree)
        (set-whole-tree-selection! tree (adjust-laddr-interlocutor link2 (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree tree) (whole-tree-selection tree)))
        (set-whole-tree-open! tree (adjust-laddr-interlocute link2 (last (whole-tree-selection Selected-tree)) tree))))
      (set-whole-tree-selection! Selected-tree LINK1ADDR)
      (set! G (graph (graph-vertices G) (replace parent-link2-t (list (triple parent-link2 (triple-edge parent-link2-t) Next-id) (triple Next-id "is defined as" link2) (triple Next-id "has env" (common-ancestor LINK1 link2))) (graph-edges G))))
      (set! G (graph (graph-vertices G) (replace parent-link1-t (list (triple LINK1PARENT (triple-edge parent-link1-t) Next-id)) (graph-edges G))))
      (set! Next-id (+ 1 Next-id))))))))

(define (common-ancestor id1 id2)
 (with
  ((find-first-overlap (ancestors id1 (list id1)) (ancestors id2 (list id2))))

  (ancestors (id l)
   (let* ((has-child (graph-neighborhood-edge-backward G id "has child"))
          (has-env (graph-neighborhood-edge-forward G id "has env"))
          (has-parent (if (null? has-child) has-env has-child)))
    (if (null? has-parent)
     l
     (ancestors (triple-start (car has-parent)) (append l (list (triple-start (car has-parent))))))))

  (find-first-overlap (l1 l2)
   (if (null? l1)
    #f
    (if (member (car l1) l2)
     (car l1)
     (find-first-overlap (cdr l1) l2))))))

(define (interlocute-lambda function? event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (has-child (member (triple parent-id "has child" id) (graph-edges G)))
        (parent-link-t (if has-child (car has-child) '())))
  (undo-push)
  (for-all-trees
   (lambda (tree)
    (set-whole-tree-open! tree (adjust-laddr-interlocute id (last (whole-tree-selection Selected-tree)) tree))))
  (set! G (graph (graph-vertices G) (replace parent-link-t
                                             (if function?
                                              (list (triple parent-id (triple-edge parent-link-t) Next-id) (triple Next-id "is defined as" id) (triple Next-id "has env" parent-id) (triple Next-id "is function" id))
                                              (list (triple parent-id (triple-edge parent-link-t) Next-id) (triple Next-id "is defined as" id) (triple Next-id "has env" parent-id)))
                                             (graph-edges G))))
  (set! Next-id (+ 1 Next-id))
  (update-data)
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
        (child (member (triple parent-id "has child" id) (graph-edges G))))
  (with
   ((if (not child)
     '()
     (begin
      (undo-push)
      (update-open)
      (update-selection)
      (remove-child)
      (update-data)
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

(define (reify-code event) (display (reify G 0 #t)) (newline))

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
   (update-data)
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
   (update-data)
   (update-childfuncs child-fun))))

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
 (let ((regex (regexp text))
       (texts (graph-filter G (lambda (t) (or (equal? (triple-edge t) "is written") (equal? (triple-edge t) "is named"))))))
  (filter (lambda (t) (regexp-match? regex (format "~a" (triple-end t)))) texts)))

(add-key-evs (list #\space add-sibling
                   #\( add-child
                   #\i insert-text
                   #\v (curry interlocute-lambda #f)
                   #\V add-var
                   #\d delete-link
                   #\r reify-code
                   #\s set-scope
                   #\L (curry interlocute-lambda #t)
                   #\a argify
                   #\u undo-pop
                   #\R redo-pop
                   #\/ search
                   'f2 write-to-file
                   'insert handle-insert
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

(define (reify g id tracing?)
 (let ((has-child (graph-neighborhood-edge-forward g id "has child"))
       (is-written (is-written-t id))
       (is-defined-as (graph-neighborhood-edge-forward g id "is defined as"))
       (is-function (graph-neighborhood-edge-forward g id "is function"))
       (has-env (graph-neighborhood-edge-backward g id "has env"))
       (is-reified-as (graph-neighborhood-edge-forward g id "is reified as")))
  (let* ((meat (cond
                ((not (null? has-child))
                 (let ((realmeat (map (curry (curryr reify (and tracing? (not (eq? (let ((ress (reify g (triple-end (car has-child)) #f))) (display ress) (newline) ress) 'quote)))) g) (map triple-end has-child))))
                  (if tracing?
                   `(let ((res ,realmeat))
                     (hash-set! h ,id (cons res (hash-ref! h ,id (quote ()))))
                     res)
                   realmeat)))
                ((not (null? is-defined-as))
                 (string->symbol (format "v~a" id)))
                ((not (null? is-reified-as))
                 (triple-end (car is-reified-as)))
                (is-written
                 (triple-end is-written))
                (#t (begin (display "unable to categorize id:  ") (display id) (newline) 'unknown)))))
   (if (null? has-env)
    meat
    (let ((env (list 'letrec
                     (map
                      (lambda (t) (list (string->symbol (format "v~a" (triple-start t))) (let* ((in-id (triple-end (car (graph-neighborhood-edge-forward g (triple-start t) "is defined as"))))
                                                                                            (is-function-in (graph-neighborhood-edge-forward g (triple-start t) "is function")))
                                                                                      (if (null? is-function-in)
                                                                                       (reify g in-id tracing?)
                                                                                       (list 'lambda (map (compose (curry format "a~s") triple-end) (graph-neighborhood-edge-forward g (triple-start t) "has formal arg")) (reify g in-id tracing?))))))
                      has-env)
                     meat)))
      env)))))


;  (string-append
;   (if (null? has-env)
;    ""
;    (string-append
;     "(letrec ("
;     (apply string-append
;      (map
;       (lambda (t) (format "(v~s ~a)" (triple-start t) (let* ((in-id (triple-end (car (graph-neighborhood-edge-forward g (triple-start t) "is defined as"))))
;                                                              (is-function-in (graph-neighborhood-edge-forward g (triple-start t) "is function")))
;                                                        (if (null? is-function-in)
;                                                         (reify g in-id)
;                                                         (format "(lambda (~a) ~a)" (string-join (map (compose (curry format "a~s") triple-end) (graph-neighborhood-edge-forward g (triple-start t) "has formal arg")) " ") (reify g in-id))))))
;       has-env))
;     ") "))
;   (if (not (null? has-child))
;    (string-join
;     (map (curry reify g) (map triple-end has-child))
;     " "
;     #:before-first "("
;     #:after-last ")")
;    (if (not (null? is-defined-as))
;     (format "v~s" id)
;     (if (not (null? is-reified-as))
;      (format "~s" (triple-end (car is-reified-as)))
;      (if is-written
;       (format "~s" (triple-end is-written))
;       (begin (display "unable to categorize id:  ") (display id) (newline))))))
;   (if (null? has-env)
;    ""
;    ")"))))

(define (test->graph->file filename)
 (graph->file (string->graph (file->string filename))))

;(test->graph->file "testdata2")

(define (yup)
 (display-on-screen 0 330 WIDTH (- HEIGHT 330) (list 0 'list '() '() '() '() '()) child-fun)
 (display "um, so yeah\n"))
 
(define (is-named-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is named")))
  (if (null? nbhd)
   #f
   (car nbhd))))

(define (child-fun a)
 (map
  (compose get-rep triple-end)
  (append
   (graph-neighborhood-edge-forward G (car a) "has child")
   (graph-neighborhood-edge-forward G (car a) "is defined as")
   (graph-neighborhood-edge-forward G (car a) "has formal arg"))))

(define (get-rep id)
 (with
  ((append (list id) (get-written id) (list (nei) (get-free-variables) (get-bound-variables) (lex-par))))

  (nei ()
   (map triple->list (append (graph-neighborhood-forward G id) (graph-neighborhood-backward G id))))

  (get-free-variables ()
   (hash-ref free-variables id))

  (get-bound-variables ()
   (hash-ref bound-variables id))

  (lex-par ()
   (let ((parent (lexical-parent id)))
    (cons parent (cadr (get-written parent)))))))


(define (get-written id)
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

(define (graph-ids g) (remove-duplicates (map triple-start (graph-edges g))))
(define (lexical-parent id)
 (let ((is-defined-as (graph-neighborhood-edge-backward G id "is defined as"))
       (has-child (graph-neighborhood-edge-backward G id "has child"))
       (has-env (graph-neighborhood-edge-forward G id "has env")))
  (cond
   ((not (null? is-defined-as))
    (triple-end (car (graph-neighborhood-edge-forward G (triple-start (car is-defined-as)) "has env"))))
   ((not (null? has-env))
    (triple-end (car has-env)))
   ((not (null? has-child))
    (triple-start (car has-child)))
   (#t '()))))

(define (package-up id2)
 (list id2 (cadr (get-written id2))))

(define free-variables (hash))
(define bound-variables (hash))

(define (update-data)
 (set! free-variables (hash))
 (set! bound-variables (hash))
 (update-free-variables)
 (update-bound-variables))

(define (update-free-variables)
 (with
  ((for-each
    update-free-variable-id
    (graph-ids G)))

  (update-free-variable-id (id)
   (with
    ((if (hash-has-key? free-variables id)
      '()
      (set! free-variables (hash-set free-variables id (set-map (set-subtract (list->set (free-from-children)) (list->set (defined-here))) package-up)))))

    (free-from-children ()
     (remove-duplicates
      (flatten
       (append
        (get-free-variables-from-children)
        (get-new-free-variables)))))

    (get-free-variables-from-children ()
     (map
      (lambda (child) (update-free-variable-id child) (map (lambda (p) (car p)) (hash-ref free-variables child)))
      (append
       (map triple-end (graph-neighborhood-edge-forward G id "has child"))
       (map triple-start (graph-neighborhood-edge-backward G id "has env")))))

    (get-new-free-variables ()
     (append
      (map triple-start (graph-neighborhood-edge-forward G id "is defined as"))
      (if (null? (graph-neighborhood-edge-forward G id "is reified as")) '() (list id))))

    (defined-here ()
      (append
       (map triple-start (graph-neighborhood-edge-backward G id "has env"))
       (map triple-end (graph-neighborhood-edge-forward G id "has formal arg"))))))))

(define (update-bound-variables)
 (with
  ((for-each
    update-bound-variable-id
    (graph-ids G)))

  (update-bound-variable-id (id)
   (with
    ((if (hash-has-key? bound-variables id)
      '()
       (set! bound-variables (hash-set bound-variables id (map package-up (bound-from-parent))))))

    (bound-from-parent ()
     (let ((parent (lexical-parent id)))
      (append
       (if (null? parent) '() (begin (update-bound-variable-id parent) (map (lambda (p) (car p)) (hash-ref bound-variables parent))))
       (map triple-start (graph-neighborhood-edge-backward G id "has env"))
       (map triple-end (graph-neighborhood-edge-forward G parent "has formal arg")))))))))

(update-data)

(yup)

