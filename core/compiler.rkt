#lang racket

(require "gnode.rkt")

(provide reify)

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(define (id->sym id)
 (string->symbol (format "v~a" id)))

(define (reify g id tracing?)
 (calc-free-variables g)
 (let reify-loop ((id id))
  (let ((gn (hash-ref g id)))
   (cond
    ((variable-gnode? gn) (id->sym id))
    ((terminal-gnode? gn) (gnode-name gn))
    ((argument-gnode? gn) (id->sym id))
    ((parent-gnode? gn)
     (let* ((childs (parent-gnode-childs gn))
            (childreifieds (map reify-loop childs))
            (vars (topo-sort (parent-gnode-vars gn)))
            (varsyms (map id->sym vars))
            (ress (map (lambda (var)
                        (let ((defined (variable-gnode-defined (hash-ref g var))))
                         (if (function-gnode? (hash-ref g var))
                          (let* ((args (function-gnode-args (hash-ref g var)))
                                 (argsyms (map id->sym args)))
                           `(lambda ,argsyms ,(reify-loop defined)))
                          (reify-loop defined))))
                       vars)))
      (if (null? varsyms)
       childreifieds
       `(letrec ,(map list varsyms ress) ,childreifieds))))))))

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
   (let ((gn (hash-ref g id)))
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
       (cond
        ((parent-gnode? gn) (parent-gnode-childs gn))
        ((variable-gnode? gn) (list (variable-gnode-defined gn)))
        (#t '()))))

     (get-new-free-variables ()
      (if (or (variable-gnode? gn) (argument-gnode? gn))
       (list id)
       '()))

     (defined-here ()
      (cond
       ((parent-gnode? gn) (parent-gnode-vars gn))
       ((function-gnode? gn) (function-gnode-args gn))
       (#t '()))))))))

