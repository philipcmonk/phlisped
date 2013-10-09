#lang racket

(require "../core/common.rkt")
(require (except-in "../core/extractdata.rkt" with))
(require "../core/gnode.rkt")
(require "../core/disp.rkt")

(provide data)

(define (paste event)
 (enter-paste-mode))

(define (handle-paste event)
 (with
  ((let ((c (send event get-key-code)))
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
		       (set-G (hash-set G parent-id (parent-gnode parent-id (gnode-name pgn) (replace child-id (list child-id (pop-clipboard)) (parent-gnode-childs pgn)) (parent-gnode-vars pgn))))))
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
			  (set-G (hash-set G parent-id (parent-gnode parent-id (gnode-name pgn) (cons (pop-clipboard) (parent-gnode-childs pgn)) (parent-gnode-vars pgn))))))
    #:open-updater      (lambda ()
                         (set-whole-tree-open! Selected-tree (set-union (whole-tree-open Selected-tree) (set parent-laddr))))
    #:selection-updater (lambda ()
                         (semantic-go 'down Selected-tree))))))

(define data
 (list
  #\p paste
  'paste handle-paste))

