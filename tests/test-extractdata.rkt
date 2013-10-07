#lang racket

(require rackunit
         racket/gui
         "../core/disp.rkt"
         "../core/extractdata.rkt")

(test-case
 "basic functionality"
 (read-file "testdata")
 (check-not-false (hash-has-key? G 0) "has no root element")
 (check-false (hash-has-key? G -1) "failed basic sanity check"))

(define (type c)
 (send Thecanvas on-char (new key-event% (key-code c))))

(define (clean)
 (read-file "testdata")
 (updater)
 (set-whole-tree-open! Selected-tree (set))
 (set-whole-tree-selection! Selected-tree '()))

(type #\tab)

(test-case
 "read-only manipulation"
 (clean)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 0 "not starting with all nodes closed")
 (check-eq? (whole-tree-selection Selected-tree) '() "not starting with correct selection")
 (type #\O)
 (check-not-eq? (set-count (whole-tree-open Selected-tree)) 0 "deep open not opening any nodes")
 (check-eq? (set-count (whole-tree-open Selected-tree)) 3 "deep open opening incorrect number of nodes")
 (type #\O)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 3 "consecutive deep opens opening more nodes")
 (type #\j)
 (check-equal? (whole-tree-selection Selected-tree) '(0) "move down not moving to correct address")
 (type #\l)
 (type #\l)
 (check-equal? (whole-tree-selection Selected-tree) '(2) "move right not moving to correct address")
 (type #\h)
 (check-equal? (whole-tree-selection Selected-tree) '(1) "move left not moving to correct address")
 (type #\k)
 (check-equal? (whole-tree-selection Selected-tree) '() "move up not moving to correct address")
 (type #\0)
 (check-equal? (whole-tree-selection Selected-tree) '(0) "move to nth child not moving to correct address")
 (type #\k)
 (type #\1)
 (check-equal? (whole-tree-selection Selected-tree) '(1) "move to nth child not moving to correct address")
 (type #\k)
 (type #\2)
 (check-equal? (whole-tree-selection Selected-tree) '(2) "move to nth child not moving to correct address")
 (type #\k)
 (type #\j)
 (type #\o)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 4 "shallow open not opening any nodes")
 (type #\O)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 38 "deep open opening incorrect number of nodes")
 (type #\0)
 (type #\2)
 (type #\O)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 57 "deep open opening incorrect number of nodes")
 (type #\c)
 (type #\c)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 40 "shallow close closing incorrect number of nodes")
 (type #\O)
 (type #\C)
 (check-eq? (set-count (whole-tree-open Selected-tree)) 38 "deep close closing incorrect number of nodes")
 )

(test-case
 "add sibling"
 (clean)
 (type #\O)
 (type #\j)
 (check-equal? (whole-tree-open Selected-tree) (set '() '(1) '(2)) "not starting with correct open nodes")
 (type #\space)
 (check-equal? (whole-tree-open Selected-tree) (set '() '(1) '(2) '(3)) "after adding sibling, not correct open nodes")
 (test-begin
  (clean)
  (type #\O)
  (type #\0)
  (type #\O)
  (type #\0)
  (type #\2)
  (type #\O)
  (type #\0)
  (type #\2)
  (type #\2)
  (type #\0)
  (type #\O)
  (type #\0)
  (type #\1)
  (type #\0)
  (type #\1)
  (type #\space)
  (check-equal? (whole-tree-open Selected-tree)
                (list->set '(() (0) (1) (2) (0 0) (0 2) (0 1) (0 0 0) (0 0 1) (0 0 2) (0 0 3) (0 0 3 0) (0 0 3 1) (0 0 3 3) (0 0 2 0) (0 0 1 2) (0 0 1 0) (0 0 1 1) (0 0 2 0 2 2 0 0) (0 0 2 0 2 2 0 1) (0 0 2 0 2 2 0 2) (0 0 2 0 2 2 0 3) (0 0 2 0 2 2 0 0 1 0 3 2) (0 0 2 0 2 2 0 0 1 0 3 1) (0 0 2 0 2 2 0 0 1 0 3 0) (0 0 3 1 2 1 1 0) (0 0 3 1 2 1 1 1) (0 0 2 0 2 2 0 0 6 0 2 2) (0 0 2 0 2 2 0 0 6 0 2 0) (0 0 2 0 2 2 0 0 6 0 2 1) (0 0 2 0 2 2 0 0 1 1) (0 0 2 0 2 2 0 0 1 0) (0 0 2 0 2 2 0 0 2 0) (0 0 2 0 2 2 0 0 2 1) (0 0 2 0 2 2 0 0 4 0) (0 0 2 0 2 2 0 0 4 1) (0 0 2 0 2 2 0 0 5 0) (0 0 2 0 2 2 0 0 5 1) (0 0 2 0 2 2 0 0 3 0) (0 0 2 0 2 2 0 0 3 1) (0 0 2 0 2 2 0 0 6 1) (0 0 2 0 2 2 0 0 6 0) (0 0 2 0 2 2 0 0 2) (0 0 2 0 2 2 0 0 1) (0 0 2 0 2 2 0 0 0) (0 0 2 0 2 2 0 0 6) (0 0 2 0 2 2 0 0 5) (0 0 2 0 2 2 0 0 4) (0 0 2 0 2 2 0 0 3) (0 0 2 0 2 2 0 0 5 0 2 0) (0 0 2 0 2 2 0 0 5 0 2 2) (0 0 2 0 2 2 0 0 5 0 2 1) (0 0 3 1 2 1 1) (0 0 3 1 2 1 0) (0 0 3 1 1 1 0) (0 0 2 0 2 2 0 0 3 0 2 2) (0 0 2 0 2 2 0 0 3 0 2 1) (0 0 2 0 2 2 0 0 3 0 2 0) (0 0 3 3 1 1 1 0) (0 0 3 3 1 1 0) (0 0 3 3 1 1 1) (0 0 2 0 2 2 0 0 4 0 2 0) (0 0 2 0 2 2 0 0 4 0 2 1) (0 0 2 0 2 2 0 0 4 0 2 2) (0 0 3 1 1 1) (0 0 3 1 1 0) (0 0 3 1 2 0) (0 0 3 1 2 1) (0 0 3 3 1 0) (0 0 3 3 1 1) (0 0 2 0 2 2 0) (0 0 2 0 2 2 1) (0 0 2 0 2 2 2) (0 0 2 0 2 2 3) (0 0 2 0 2 1 1) (0 0 2 0 2 1 2) (0 0 2 0 2 1 0) (0 0 2 0 1 1 0) (0 0 2 0 1 1 1) (0 0 2 0 1 1 2) (0 0 2 0 2 2 0 0 2 0 2 2) (0 0 2 0 2 2 0 0 2 0 2 1) (0 0 2 0 2 2 0 0 2 0 2 0) (0 0 2 0 2 0) (0 0 2 0 2 1) (0 0 2 0 2 2) (0 0 2 0 1 0) (0 0 2 0 1 1) (0 0 3 1 2 1 1 1 0) (0 0 2 0 2 2 0 0 1 1 3) (0 0 2 0 2 2 0 0 1 1 0) (0 0 2 0 2 2 0 0 1 0 0) (0 0 2 0 2 2 0 0 1 0 3) (0 0 2 0 2 2 0 0 1 0 2) (0 0 2 0 2 2 0 0 6 0 2) (0 0 2 0 2 2 0 0 6 0 0) (0 0 2 0 2 2 0 0 6 1 2) (0 0 2 0 2 2 0 0 6 1 0) (0 0 2 0 2 2 0 0 5 1 0) (0 0 2 0 2 2 0 0 5 1 2) (0 0 2 0 2 2 0 0 5 0 2) (0 0 2 0 2 2 0 0 5 0 0) (0 0 2 0 2 2 0 0 3 0 0) (0 0 2 0 2 2 0 0 3 0 2) (0 0 2 0 2 2 0 0 3 1 0) (0 0 2 0 2 2 0 0 3 1 1) (0 0 2 0 2 2 0 0 4 0 2) (0 0 2 0 2 2 0 0 4 0 0) (0 0 2 0 2 2 0 0 4 1 0) (0 0 2 0 2 2 0 0 4 1 1) (0 0 2 0 2 2 0 0 2 0 2) (0 0 2 0 2 2 0 0 2 0 0) (0 0 2 0 2 2 0 0 2 1 3) (0 0 2 0 2 2 0 0 2 1 0) (0 0 3 1 2) (0 0 3 1 1) (0 0 3 1 0) (0 0 3 3 1) (0 0 3 3 0) (0 0 2 0 0) (0 0 2 0 1) (0 0 2 0 2) (0 0 1 2 0) (0 0 1 1 0)))
                "add sibling next to variable leaving wrong open set"))
 )


(exit)
