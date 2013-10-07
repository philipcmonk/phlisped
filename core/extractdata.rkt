#lang racket

(require "graph.rkt" "disp.rkt" "compiler.rkt")
(require racket/set)

(provide Thecanvas Info (all-defined-out) update-childfuncs for-all-trees semantic-go)

(define GRFILE "visualizations/treemap-v11n.phl")

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

(define G '())
(define (set-G g) (set! G g))

(define (read-file filename)
 (set! G (call-with-input-file filename (lambda (f) (read f)))))

(read-file GRFILE)

(define INSERTTEXT "")
(define LINK1 '())
(define LINK1PARENT '())
(define LINK1ADDR '())

(define runtime-vals (hash))
(define (set-runtime-vals vals) (set! runtime-vals vals))

(set! Next-id (triple-end (car (graph-neighborhood-edge-forward G 'next-id "is"))))
(define (set-Next-id id) (set! Next-id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     Graph Changers                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (updater
         #:undoer (undoer undo-push)
         #:graph-changer (graph-changer values)
         #:open-updater (open-updater values)
         #:data-updater (data-updater update-data)
         #:childfuncs-updater (childfuncs-updater (lambda () (update-childfuncs child-fun)))
         #:selection-updater (selection-updater values))
 (undoer)
 (graph-changer)
 (open-updater)
 (data-updater)
 (childfuncs-updater)
 (selection-updater))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                   Laddr Adjustments                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjust-laddr id parent-id pos u laddr)
 (if (null? laddr)
  '()
  (if (and (> (car laddr) pos)
           (eq? parent-id (car (node-data (utterance-node u)))))
;           (eq? id (car (node-data (utterance-node (list-ref (utterance-args u) pos))))))
   (cons (+ 1 (car laddr)) (adjust-laddr id parent-id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr)))
   (cons (car laddr) (adjust-laddr id parent-id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

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
   (cons (car laddr) (cons 0 (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))
   (cons (car laddr) (adjust-laddr-interlocutor id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr))))))

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
   whole-laddr
   (remove-laddr-del id pos (list-ref (utterance-args u) (car laddr)) (cdr laddr) whole-laddr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     Miscellaneous                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Little '-t' Getters                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (is-named-t id)
 (let ((nbhd (graph-neighborhood-edge-forward G id "is named")))
  (if (null? nbhd)
   #f
   (car nbhd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             Utility Functions                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (selected-id tree)
 (car (node-data (utterance-node (whole-tree-selection-u tree)))))

(define (selected-parent-id tree)
 (car (node-data (utterance-node (utterance-parent (whole-tree-selection-u tree) tree)))))

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

(define (graph-ids g) (hash-keys G))

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

(define (graph->file g)
 (let ((g (graph-replace-edges G (car (graph-neighborhood-edge-forward g 'next-id "is")) (list (triple 'next-id "is" Next-id)))))
  (call-with-output-file GRFILE #:exists 'truncate (lambda (f) (write g f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 Undo/Redo                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define UNDOSTACK '())
(define REDOSTACK '())
(define (set-UNDOSTACK stack) (set! UNDOSTACK stack))
(define (set-REDOSTACK stack) (set! REDOSTACK stack))

(define (undo-push)
 (set! REDOSTACK '())
 (set! UNDOSTACK (cons (list G (whole-tree-open Selected-tree) (whole-tree-selection Selected-tree)) UNDOSTACK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 Clipboard                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Clipboard #f)

(define (push-clipboard id)
 (set! Clipboard id))

(define (pop-clipboard) Clipboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  Child-fun and Related                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (child-fun a)
 (map
  (compose get-rep triple-end)
  (append
   (graph-neighborhood-edge-forward G (car a) "has child")
   (graph-neighborhood-edge-forward G (car a) "is defined as")
   (graph-neighborhood-edge-forward G (car a) "has formal arg"))))

(define (get-rep id)
 (with
  ((append (list id) (get-written id) (list (nei) (get-free-variables) (get-bound-variables) (lex-par) (runtime-values))))

  (nei ()
   (map triple->list (append (graph-neighborhood-forward G id) (graph-neighborhood-backward G id))))

  (get-free-variables ()
   (hash-ref free-variables id))

  (get-bound-variables ()
   (hash-ref bound-variables id))

  (lex-par ()
   (let ((parent (lexical-parent id)))
    (cons parent (cadr (get-written parent)))))
  
  (runtime-values ()
   (let* ((reses (map
                  (curry triple-end)
                  (graph-neighborhood-edge-forward runtime-vals id 'has-res)))
          (vals (map
                 (compose (curry map triple-end) (curryr (curry graph-neighborhood-edge-forward runtime-vals) 'has-val))
                 reses)))
    (map
     (lambda (res val)
      (let* ((root-reses (map triple-end (graph-neighborhood-edge-forward runtime-vals res 'has-roots)))
             (root-nodes (map
                          (compose (curry triple-start) safe-car (curryr (curry graph-neighborhood-edge-backward runtime-vals) 'has-res))
                          root-reses))
             (root-vals (map
                         (compose (curry triple-end) safe-car (curryr (curry graph-neighborhood-edge-forward runtime-vals) 'has-val))
                         root-reses)))
       (cons res (cons val (cons root-reses (cons root-nodes (cons root-vals '())))))))
      reses
      vals)))))

(define (safe-car l)
 (if (null? l)
  '()
  (car l)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                   Updating and Related                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (begin
       (set! free-variables (hash-set free-variables id '()))
       (set! free-variables (hash-set free-variables id (set-map (set-subtract (list->set (free-from-children)) (list->set (defined-here))) package-up))))))

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
       (map triple-end (graph-neighborhood-edge-forward G id "is defined as")))))
;       (map triple-start (graph-neighborhood-edge-backward G id "has env")))))

    (get-new-free-variables ()
     (append
      (map triple-start (graph-neighborhood-edge-forward G id "is defined as"))
;      (map (lambda (child) (update-free-variable-id (triple-end child)) (map car (hash-ref free-variables (triple-end child)))) (graph-neighborhood-edge-forward G id "is defined as"))
      (if (null? (graph-neighborhood-edge-forward G id "is reified as")) '() (list id))))

    (defined-here ()
     (append
      (map triple-start (graph-neighborhood-edge-backward G id "has env"))
;      (map (compose update-free-variable-id triple-start) (graph-neighborhood-edge-backward G id "has env"))
      (map triple-end (graph-neighborhood-edge-forward G id "has formal arg"))))))))
;      (map (compose update-free-variable-id triple-end) (graph-neighborhood-edge-forward G id "has formal arg"))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                           Go                                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (yup)
 (display-on-screen 0 330 WIDTH (- HEIGHT 330) (list 0 'list '() '() '() '() '() '()) child-fun)
 (display "um, so yeah\n"))

;(display (graph->string G))
(if NEWCODE
 (graph->file G)
 '())

(update-data)

(yup)

