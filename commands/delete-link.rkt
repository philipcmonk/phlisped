#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with))
(require "../core/graph.rkt")

(provide data)

(define (delete-link event)
 (let* ((id (selected-id Selected-tree))
        (parent-id (selected-parent-id Selected-tree))
        (child (member (triple parent-id "has child" id) (graph-neighborhood-forward G parent-id))))
  (with
   ((if (not child)
     '()
     (updater
      #:graph-changer (lambda ()
                       (cond
                         (child
                           (push-clipboard (triple-end (car child)))
                           (set-G (graph-remove-edge G (car child))))
                         (#t '())))
      #:open-updater  (lambda ()
                       (update-open)
                       (update-selection)))))

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
        (curry adjust-laddr-del id (last (whole-tree-selection Selected-tree)) (whole-tree-utterance-tree tree))))))))))

(define data
 (list #\d delete-link))

