#lang racket

(require "graph.ss")

(provide reify)

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(define (reify g id tracing?)
 (calc-free-variables g)
 (let reify-loop ((id id) (tracing? tracing?))
  (let ((has-child (graph-neighborhood-edge-forward g id "has child"))
        (is-written (graph-neighborhood-edge-forward g id "is written"))
        (is-defined-as (graph-neighborhood-edge-forward g id "is defined as"))
        (is-function (graph-neighborhood-edge-forward g id "is function"))
        (has-env (graph-neighborhood-edge-backward g id "has env"))
        (is-reified-as (graph-neighborhood-edge-forward g id "is reified as")))
   (let* ((meat (cond
                 ((not (null? has-child))
                  (let ((realmeat (map (curryr reify-loop (and tracing? (not (eq? (let ((ress (reify-loop (triple-end (car has-child)) #f))) (display ress) (newline) ress) 'quote)))) (map triple-end has-child))))
                   (if tracing?
                    `(begin
                      (set! stack (cons (quote ,(string->symbol (format "flag~a" id))) stack))
                      (let ((res ,realmeat)
                            (childreses '()))
                       (andmap
                        (lambda (item) (if (eq? item (quote ,(string->symbol (format "flag~a" id)))) #f (begin (set! childreses (cons item childreses)) #t)))
                        stack)
                       (set! h (graph-append-edges h (list (triple ,id 'has-res Next-r) (triple Next-r 'has-val res))))
                       (set! h (graph-append-edges h (map (curry triple Next-r 'has-roots) childreses)))
                       (set! stack (drop stack (+ 1 (length childreses))))
                       (set! stack (cons Next-r stack))
                       (set! Next-r (- Next-r 1))
                       res))
                    realmeat)))
                 ((not (null? is-defined-as))
                  (string->symbol (format "v~a" id)))
                 ((not (null? is-reified-as))
                  (triple-end (car is-reified-as)))
                 ((not (null? is-written))
                  (triple-end (car is-written)))
                 (#t (begin (display "unable to categorize id:  ") (display id) (newline) 'unknown)))))
    (if (null? has-env)
     meat
     (let ((env (list 'letrec
                      (map
                       (lambda (t) (list (string->symbol (format "v~a" t)) (let* ((in-id (triple-end (car (graph-neighborhood-edge-forward g t "is defined as"))))
                                                                                  (is-function-in (graph-neighborhood-edge-forward g t "is function")))
                                                                            (if (null? is-function-in)
                                                                             (reify-loop in-id tracing?)
                                                                             (list 'lambda (map (compose string->symbol (curry format "a~s") triple-end) (graph-neighborhood-edge-forward g t "has formal arg")) (reify-loop in-id tracing?))))))
                       (topo-sort (map triple-start has-env)))
                      meat)))
       env))))))

(define (topo-sort ids)
 (define traversed '())
 (with
  ((reverse (foldl topo '() ids)))

  (topo (id stack)
   (if (or (member id traversed) (not (member id ids)))
    stack
    (begin
     (set! traversed (cons id traversed))
     (if (null? (hash-ref free-variables id))
      (cons id stack)
      (cons id (foldl topo stack (hash-ref free-variables id)))))))))

(define free-variables (hash))

(define (calc-free-variables g)
 (with
  ((for-each
    update-free-variable-id
    (hash-keys g)))

  (update-free-variable-id (id)
   (with
    ((if (hash-has-key? free-variables id)
      '()
      (begin
       (set! free-variables (hash-set free-variables id '()))
       (set! free-variables (hash-set free-variables id (set->list (set-subtract (list->set (free-from-children)) (list->set (defined-here)))))))))

    (free-from-children ()
     (remove-duplicates
      (flatten
       (append
        (get-free-variables-from-children)
        (get-new-free-variables)))))

    (get-free-variables-from-children ()
     (map
      (lambda (child) (update-free-variable-id child) (hash-ref free-variables child))
      (append
       (map triple-end (graph-neighborhood-edge-forward g id "has child"))
       (map triple-end (graph-neighborhood-edge-forward g id "is defined as")))))

    (get-new-free-variables ()
     (append
      (map triple-start (graph-neighborhood-edge-forward g id "is defined as"))
      (if (null? (graph-neighborhood-edge-forward g id "is reified as")) '() (list id))))

    (defined-here ()
     (append
      (map triple-start (graph-neighborhood-edge-backward g id "has env"))
      (map triple-end (graph-neighborhood-edge-forward g id "has formal arg"))))))))

