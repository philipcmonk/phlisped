#lang racket

(require "../core/common.rkt")
(require "../core/commands-common.rkt")
(require "../core/extractdata.rkt")
(require "../core/gnode.rkt")
(require "../core/disp.rkt")

(provide data handle-paste)

(define (paste event)
 (enter-paste-mode))

(define (handle-paste event (char '()))
 (with
  ((let ((c (if (null? char) (send event get-key-code) char)))
    (cond
     ((eq? c #\j)
      (make-paste-below (selected-id Selected-tree) (whole-tree-selection Selected-tree))
      (exit-paste-mode))
     ((eq? c #\h)
      (let* ((parent (selected-parent-id Selected-tree))
             (child (selected-id Selected-tree)))
       (make-paste parent child))
      (exit-paste-mode))
     ((eq? c #\l)
      (let* ((parent (selected-parent-id Selected-tree))
             (child (selected-id Selected-tree)))
       (make-paste parent child))
      (exit-paste-mode))
     ((eq? c 'escape)
      (exit-paste-mode))
     (#t '()))))

  (make-paste (parent-id child-id)
   (if (member child-id (parent-gnode-childs (hash-ref G parent-id)))
    (updater
     #:graph-changer (lambda ()
                      (let ((pgn (hash-ref G parent-id)))
		       (set-G (graph-add-child-after G parent-id child-id (pop-clipboard)))))
;		       (set-G (hash-set G parent-id (parent-gnode parent-id (gnode-name pgn) (replace child-id (list child-id (pop-clipboard)) (parent-gnode-childs pgn)) (parent-gnode-vars pgn))))))
     #:open-updater  (lambda ()
                      (for-all-trees
                       (lambda (tree)
                        (set-whole-tree-open! tree
                                              (set-union
                                               (list->set
                                                (set-map
                                                 (whole-tree-open tree)
                                                 (curry adjust-laddr child-id parent-id
                                                        (last (whole-tree-selection Selected-tree))
                                                        (whole-tree-utterance-tree tree))))
                                               (set (append (drop-right (whole-tree-selection Selected-tree) 1) (list (+ 1 (last (whole-tree-selection Selected-tree)))))))))))
     #:selection-updater (lambda () (semantic-go 'right Selected-tree)))
    '()))

  (make-paste-below (parent-id parent-laddr)
   (updater
    #:graph-changer     (lambda ()
                         (let ((pgn (hash-ref G parent-id)))
			  (set-G (graph-add-child-beg G parent-id (pop-clipboard)))))
    #:open-updater      (lambda ()
                         (set-whole-tree-open! Selected-tree (set-union (whole-tree-open Selected-tree) (set parent-laddr))))
    #:selection-updater (lambda ()
                         (semantic-go 'down Selected-tree))))))

(define data
 (list
  '(#\p enter-paste) paste
  '(paste) handle-paste))

