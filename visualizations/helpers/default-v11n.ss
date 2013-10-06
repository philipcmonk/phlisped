#lang racket

(require sgl sgl/gl)
(require "../../common.ss")
(require "def-painter.ss")

(provide make-default-v11n)

(define (make-default-v11n #:child-w-generator (child-w-generator values) #:child-x-generator (child-x-generator values) #:child-y-generator (child-y-generator values) #:update-x-generator (update-x-generator values) #:update-y-generator (update-y-generator values) #:width-generator (width-generator values) #:height-generator (height-generator values))
 (v11n
  (def-painter
   #:drawer
    (lambda (text x y w h text-w text-h clr u tree center)
     (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
     (if (and (<= (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
              (<= (/ text-h (whole-tree-zoom tree)) h))
      (draw-text
       text
       (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
       (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
       (car clr)
       tree)
      '())))

  (lambda (n x y w row siblings tree)
   (let node->utterance ((n n) (x x) (y y) (w w) (row row) (siblings siblings) (tree tree))
    (with
     ((let ((children 
             (if (closed? n tree)
              '()
              (let ((child-w (child-w-generator n)))
               (caddr
                (foldl
                 (lambda (arg data)
                  (let ((res (node->utterance
                              arg
                              (child-x-generator data w n)
                              (child-y-generator data arg tree)
                              child-w
                              (+ 1 row)
                              (- (length (node-args n)) 1)
                              tree)))
                   (list
                    (update-x-generator data res)
                    (update-y-generator data res arg tree)
                    (if (null? res)
                     (caddr data)
                     (append
                      (caddr data)
                      (list res))))))
                 (list x y '())
                 (node-args n)))))))
       (utterance
        n
        x
        y
        (width-generator w n children)
        (height-generator n tree)
        (box-width ((node-text-func n) n))
        (box-height ((node-text-func n) n))
        children
        (get-color n tree)))))))

  (lambda (root x y tree)
   (let find-utterance ((root root) (x x) (y y) (tree tree))
    (with
     ((if (or
           (above-bottom-of-utterance?)
           (utterance-is-closed?)
           (has-no-children?)
           (is-to-the-right-of-utterance?))
       root
       (pass-on-to-child)))
   
     (above-bottom-of-utterance? ()
      (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root))))
   
     (utterance-is-closed? ()
      (closed? (utterance-node root) tree))
           
     (has-no-children? ()
      (null? (node-args (utterance-node root))))
   
     (is-to-the-right-of-utterance? ()
      (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
   
     (pass-on-to-child ()
      (ormap
       (lambda (child)
        (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
         (find-utterance child x y tree)
         #f))
       (utterance-args root))))))

  (lambda (dir event)
   (cond
    ((eq? dir 'up)
     (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
    ((eq? dir 'down)
     (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
    ((eq? dir 'left)
     (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
    ((eq? dir 'right)
     (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

