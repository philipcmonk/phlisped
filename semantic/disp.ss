#lang racket

(require (except-in racket/gui yield ->))
(require (only-in racket/gui (yield yield-gui) (-> ->-gui)))
(require sgl sgl/gl)

(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector)

(define-ffi-definer define-ftgl (ffi-lib "libftgl"))

(define _FTGLfont (_cpointer 'FTGLfont))
(define-ftgl ftglCreatePixmapFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreateBitmapFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreateBufferFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreateTextureFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreateOutlineFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreatePolygonFont (_fun _path -> _FTGLfont))
(define-ftgl ftglCreateExtrudeFont (_fun _path -> _FTGLfont))
(define-ftgl ftglSetFontFaceSize (_fun _FTGLfont _int _int -> _void))
(define-ftgl ftglGetFontLineHeight (_fun _FTGLfont -> _float))
(define-ftgl ftglGetFontAdvance (_fun _FTGLfont _string -> _float))
(define-ftgl ftglRenderFont (_fun _FTGLfont _string _int -> _void))
(define-ftgl ftglDestroyFont (_fun _FTGLfont -> _void))

(provide my-canvas% box-width box-height box-maj-dim node-width node-height node-maj-dim VERTICAL display-on-screen add-to-screen Thecanvas Info Selected-tree utterance-parent utterance-node utterance-args node-data node-laddr whole-tree-selection-u whole-tree-selection set-whole-tree-selection! whole-tree-open set-whole-tree-open! whole-tree-utterance-tree add-key-evs key-evs update-childfuncs set-info enter-insert-mode exit-insert-mode enter-scope-mode exit-scope-mode enter-argify-mode exit-argify-mode enter-search-mode exit-search-mode set-search-results Search-results show-search-tree scroll-search-results remove-search-tree paint-info semantic-go find-utterance-from-laddr-safe for-all-trees)

(struct node (data laddr prom-args text-func) #:transparent)
(struct utterance (node x y w h text-w text-h args clr) #:transparent)
(struct whole-tree (n-tree childfunc utterance-tree open selection x y w h v11n offset-x offset-y zoom) #:mutable)

(define (whole-tree-selection-u tree)
 (find-utterance-from-laddr-safe (whole-tree-utterance-tree tree) (whole-tree-selection tree)))

(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define VAR1 0)
(define VAR2 0)
(define VAR3 0)
(define VAR1OFFSET 1)
(define VAR2OFFSET 8)
(define VAR3OFFSET 8)
(define VAR1MIN 0)
(define VAR2MIN 0)
(define VAR3MIN 0)
(define VAR1MAX 50)
(define VAR2MAX 255)
(define VAR3MAX 255)

(define COLORSCHEME 'alternate)
;(define COLOR1 (cons '(255 255 255) '(0 0 96)))
;(define COLOR2 (cons '(255 255 255) '(48 0 96)))
;(define COLOR3 (cons '(255 255 255) '(0 48 96)))
;(define COLOR4 (cons '(255 255 255) '(48 48 96)))
;(define COLOR1 (cons '(255 255 255) '(96 0 0)))
;(define COLOR2 (cons '(255 255 255) '(96 32 0)))
;(define COLOR3 (cons '(255 255 255) '(96 72 0)))
;(define COLOR4 (cons '(255 255 255) '(96 96 0)))
;(define COLOR5 (cons '(255 255 255) '(80 0 0)))
;(define COLOR6 (cons '(255 255 255) '(80 0 0)))
(define CODECOLOR1 (cons '(255 255 255) '(255 0 0)))
(define CODECOLOR2 (cons '(255 255 255) '(223 0 0)))
(define CODECOLOR3 (cons '(255 255 255) '(191 0 0)))
;(define SELCOLOR (cons '(255 255 255) '(0 0 255)))
;(define SEL2COLOR (cons '(255 255 255) '(112 0 112)))
(define INFOCOLOR (cons '(255 255 255) '(0 0 0)))
(define BGCOLOR "black")
(define INITIALCOLOR '(0 0 127))
(define COLORRANGES '(127 0 -127))
(define FGCOLOR '(255 255 255))
(define CELLHEIGHT 25)

(define win (new frame% (label "vilisp") (min-width WIDTH) (min-height HEIGHT)))

(define (open? n tree) (set-member? (whole-tree-open tree) (node-laddr n)))
(define closed? (negate open?))
(define node-args (compose force node-prom-args))

(struct v11n (paint-tree node->v11n-utterance find-utterance wheel))
(define default-v11n (v11n
                      (lambda (tree)
                       (with
                        ((apply gl-scissor (whole-tree-dim tree))
                         (apply gl-viewport (whole-tree-dim tree))
                         (gl-matrix-mode 'projection)
                         (gl-load-identity)
                         (gl-ortho
                          (- (whole-tree-offset-x tree))
                          (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
                          (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
                          (whole-tree-offset-y tree)
                          -100.0
                          100.0)
                         (gl-matrix-mode 'modelview)
                         (gl-load-identity)
                         (utterance-paint (whole-tree-utterance-tree tree) tree))
                  
                        (utterance-paint (u tree)
                         (with
                          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
                                  (x (utterance-x u))
                                  (y (utterance-y u))
                                  (w (utterance-w u))
                                  (h (utterance-h u))
                                  (text-w (utterance-text-w u))
                                  (text-h (utterance-text-h u))
                                  (args (utterance-args u))
                                  (clr (utterance-clr u)))
                            (if (invisible? x y w h tree)
                             '()
                             (begin
                              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
                              (if (< (whole-tree-zoom tree) 1) '()
                               (draw-text
                                text
                                (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                (car clr)
                                tree))
                              (for-each (lambda (arg) (utterance-paint arg tree)) args)))))
                  
                         (invisible? (x y w h tree)
                          (or
                           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
                           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
                           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
                    
                         (center (offset lenwhole lenpiece start width)
                          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
                           (if (< visible-width lenpiece)
                            (if (< offset start)
                             (- (+ offset lenwhole) lenpiece)
                             offset)
                            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))

                      (lambda (n x y w row siblings tree)
                       (let node->utterance ((n n) (x x) (y y) (w w) (row row) (siblings siblings) (tree tree))
                        (with
                         ((let ((children 
                                 (if (closed? n tree)
                                  '()
                                  (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (node-width arg tree)) (node-args n))) -1)))
                                   (caddr
                                    (foldl
                                     (lambda (arg data)
                                      (let ((res (node->utterance
                                                  arg
                                                  (if VERTICAL (+ (car data) w) (car data))
                                                  (if VERTICAL (cadr data) (+ (cadr data) (node-height arg tree)))
                                                  child-w
                                                  (+ 1 row)
                                                  (- (length (node-args n)) 1)
                                                  tree)))
                                       (list
                                        (if VERTICAL (car data) (+ (car data) (utterance-w res)))
                                        (if VERTICAL (+ (cadr data) (node-height arg tree)) (cadr data))
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
                            (if VERTICAL w (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children))))
                            (node-height n tree)
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

(struct other-v11n-utterance utterance (total-height runtime-vals))

(define other-v11n (v11n
                      (lambda (tree)
                       (with
                        ((apply gl-scissor (whole-tree-dim tree))
                         (apply gl-viewport (whole-tree-dim tree))
                         (gl-matrix-mode 'projection)
                         (gl-load-identity)
                         (gl-ortho
                          (- (whole-tree-offset-x tree))
                          (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
                          (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
                          (whole-tree-offset-y tree)
                          -1.0
                          1.0)
                         (gl-matrix-mode 'modelview)
                         (gl-load-identity)
                         (utterance-paint (whole-tree-utterance-tree tree) tree))
                  
                        (utterance-paint (u tree)
                         (with
                          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
                                  (x (utterance-x u))
                                  (y (utterance-y u))
                                  (w (utterance-w u))
                                  (h (utterance-h u))
                                  (text-w (utterance-text-w u))
                                  (text-h (utterance-text-h u))
                                  (args (utterance-args u))
                                  (clr (utterance-clr u)))
                            (if (invisible? x y w h tree)
                             '()
                             (begin
                              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
                              (if (< (whole-tree-zoom tree) 1) '()
                               (draw-text
                                text
                                (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                (car clr)
                                tree))
                              (map
                               (lambda (val n)
                                (draw-text
                                 (format "~a" val)
                                 (+ x w 100 (* 40 n))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                 (car clr)
                                 tree))
                               (other-v11n-utterance-runtime-vals u)
                               (build-list (length (other-v11n-utterance-runtime-vals u)) identity))
                              (for-each (lambda (arg) (utterance-paint arg tree)) args)))))
                  
                         (invisible? (x y w h tree)
                          (or
                           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
                           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
                           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
                    
                         (center (offset lenwhole lenpiece start width)
                          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
                           (if (< visible-width lenpiece)
                            (if (< offset start)
                             (- (+ offset lenwhole) lenpiece)
                             offset)
                            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))

                      (lambda (n x y w row siblings tree)
                       (let node->utterance ((n n) (x x) (y y) (row row) (siblings siblings) (tree tree))
                        (let ((children 
                               (if (closed? n tree)
                                '()
                                (cadr
                                 (foldl
                                  (lambda (arg data)
                                   (let ((res (node->utterance
                                               arg
                                               (+ x 10)
                                               (car data)
                                               (+ 1 row)
                                               (- (length (node-args n)) 1)
                                               tree)))
                                    (if (null? res)
                                     (list
                                      0
                                      (cadr data))
                                     (list
                                      (+ (car data) (other-v11n-utterance-total-height res))
                                      (append
                                       (cadr data)
                                       (list res))))))
                                  (list (+ y (node-height n tree)) '())
                                  (node-args n))))))
                         (other-v11n-utterance
                          n
                          x
                          y
                          (box-width ((node-text-func n) n))
;                          (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children)))
;                          (foldl + (node-height n tree) (map utterance-h children))
                          (node-height n tree)
                          (box-width ((node-text-func n) n))
                          (box-height ((node-text-func n) n))
                          children
                          (get-color n tree)
                          (foldl + (node-height n tree) (map other-v11n-utterance-total-height children))
                          (map cadr (list-ref (node-data n) 7))))))

                      (lambda (root x y tree)
                       (let find-utterance ((root root) (x x) (y y))
                        (with
                         ((if (or
                               (< y (+ (utterance-y root) (utterance-h root)))
                               (> y (+ (utterance-y root) (other-v11n-utterance-total-height root))))
                           root
                           (ormap
                            (lambda (child)
                             (if (< y (+ (utterance-y child) (other-v11n-utterance-total-height child)))
                              (find-utterance child x y)
                              #f))
                            (utterance-args root)))))))

                      (lambda (dir event)
                       (cond
                        ((eq? dir 'up)
                         (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
                        ((eq? dir 'down)
                         (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))
                        ((eq? dir 'left)
                         (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
                        ((eq? dir 'right)
                         (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))))))

(define other2-v11n (v11n
                      (lambda (tree)
                       (with
                        ((apply gl-scissor (whole-tree-dim tree))
                         (apply gl-viewport (whole-tree-dim tree))
                         (gl-matrix-mode 'projection)
                         (gl-load-identity)
                         (gl-ortho
                          (- (whole-tree-offset-x tree))
                          (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
                          (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
                          (whole-tree-offset-y tree)
                          -1.0
                          1.0)
                         (gl-matrix-mode 'modelview)
                         (gl-load-identity)
                         (utterance-paint (whole-tree-utterance-tree tree) tree))
                  
                        (utterance-paint (u tree)
                         (with
                          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
                                  (x (utterance-x u))
                                  (y (utterance-y u))
                                  (w (utterance-w u))
                                  (h (utterance-h u))
                                  (text-w (utterance-text-w u))
                                  (text-h (utterance-text-h u))
                                  (args (utterance-args u))
                                  (clr (utterance-clr u)))
                            (if (invisible? x y w h tree)
                             '()
                             (begin
;                              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
                              (if (< (whole-tree-zoom tree) 1) '()
                               (draw-text
                                (format "(~a" text)
                                (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
                                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
                                (car clr)
                                tree))
                              (for-each (lambda (arg) (utterance-paint arg tree)) args)))))
                  
                         (invisible? (x y w h tree)
                          (or
                           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
                           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
                           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
                    
                         (center (offset lenwhole lenpiece start width)
                          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
                           (if (< visible-width lenpiece)
                            (if (< offset start)
                             (- (+ offset lenwhole) lenpiece)
                             offset)
                            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))

                      (lambda (n x y w row siblings tree)
                       (let node->utterance ((n n) (x x) (y y) (row row) (siblings siblings) (tree tree))
                        (let ((children 
                               (if (closed? n tree)
                                '()
                                (cadr
                                 (foldl
                                  (lambda (arg data)
                                   (let ((res (node->utterance
                                               arg
                                               (+ x 10)
                                               (car data)
                                               (+ 1 row)
                                               (- (length (node-args n)) 1)
                                               tree)))
                                    (if (null? res)
                                     (list
                                      0
                                      (cadr data))
                                     (list
                                      (+ (car data) (other-v11n-utterance-total-height res))
                                      (append
                                       (cadr data)
                                       (list res))))))
                                  (list (+ y (node-height n tree)) '())
                                  (node-args n))))))
                         (other-v11n-utterance
                          n
                          x
                          y
                          (box-width ((node-text-func n) n))
;                          (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children)))
;                          (foldl + (node-height n tree) (map utterance-h children))
                          (node-height n tree)
                          (box-width ((node-text-func n) n))
                          (box-height ((node-text-func n) n))
                          children
                          (get-color n tree)
                          (foldl + (node-height n tree) (map other-v11n-utterance-total-height children))
                          (map cadr (list-ref (node-data n) 7))))))

                      (lambda (root x y tree)
                       (let find-utterance ((root root) (x x) (y y))
                        (with
                         ((if (or
                               (< y (+ (utterance-y root) (utterance-h root)))
                               (> y (+ (utterance-y root) (other-v11n-utterance-total-height root))))
                           root
                           (ormap
                            (lambda (child)
                             (if (< y (+ (utterance-y child) (other-v11n-utterance-total-height child)))
                              (find-utterance child x y)
                              #f))
                            (utterance-args root)))))))

                      (lambda (dir event)
                       (cond
                        ((eq? dir 'up)
                         (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
                        ((eq? dir 'down)
                         (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))
                        ((eq? dir 'left)
                         (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
                        ((eq? dir 'right)
                         (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))))))

(struct treemap-utterance utterance (total-width total-height))

(define treemap-v11n (v11n
                      (lambda (tree)
                       (with
                        ((apply gl-scissor (whole-tree-dim tree))
                         (apply gl-viewport (whole-tree-dim tree))
                         (gl-matrix-mode 'projection)
                         (gl-load-identity)
                         (gl-ortho
                          (- (whole-tree-offset-x tree))
                          (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
                          (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
                          (whole-tree-offset-y tree)
                          -1.0
                          1.0)
                         (gl-matrix-mode 'modelview)
                         (gl-load-identity)
                         (utterance-paint (whole-tree-utterance-tree tree) tree))
                  
                        (utterance-paint (u tree)
                         (with
                          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
                                  (x (utterance-x u))
                                  (y (utterance-y u))
                                  (w (utterance-w u))
                                  (h (utterance-h u))
                                  (t-w (treemap-utterance-total-width u))
                                  (t-h (treemap-utterance-total-height u))
                                  (text-w (utterance-text-w u))
                                  (text-h (utterance-text-h u))
                                  (args (utterance-args u))
                                  (clr (utterance-clr u)))
                              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y t-w t-h)
                              (if (and (< (/ (- text-w PADDING) (whole-tree-zoom tree)) w)
                                       (< (/ text-h (whole-tree-zoom tree)) h))
                               (draw-text
                                text
                                (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree))
                                (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree)))
                                (car clr)
                                tree)
                               '())
                              (for-each (lambda (arg) (utterance-paint arg tree)) args)))
                  
                         (invisible? (x y w h tree)
                          (or
                           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
                           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
                           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
                    
                         (center (offset lenwhole lenpiece start width)
                          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
                           (if (< visible-width lenpiece)
                            (if (< offset start)
                             (- (+ offset lenwhole) lenpiece)
                             offset)
                            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))

                      (lambda (n x y w row siblings tree)
                       (let node->utterance ((n n) (x x) (y y) (w (whole-tree-w tree)) (h (whole-tree-h tree)) (dir 'horizontal) (row row) (siblings siblings) (tree tree))
                        (let ((children
                               (if (or (closed? n tree) (null? (node-args n)))
                                '()
                                (let* ((num-args (+ 1 (length (node-args n))))
                                       (child-wh (/ (if (eq? dir 'horizontal) w h) num-args)))
                                 (map
                                  (lambda (arg n)
                                   (if (eq? dir 'horizontal)
                                    (node->utterance
                                     arg
                                     (+ x (* n child-wh))
                                     y
                                     child-wh
                                     h
                                     'vertical
                                     (+ 1 row)
                                     (- num-args 1)
                                     tree)
                                    (node->utterance
                                     arg
                                     x
                                     (+ y (* n child-wh))
                                     w
                                     child-wh
                                     'horizontal
                                     (+ 1 row)
                                     (- num-args 1)
                                     tree)))
                                  (node-args n)
                                  (map (curry + 1) (build-list (length (node-args n)) identity)))))))
                         (treemap-utterance
                          n
                          x
                          y
                          (or (and (or (eq? dir 'vertical) (closed? n tree) (null? (node-args n))) w) (/ w (+ 1 (length (node-args n)))))
                          (or (and (or (eq? dir 'horizontal) (closed? n tree) (null? (node-args n))) h) (/ h (+ 1 (length (node-args n)))))
                          (box-width ((node-text-func n) n))
                          (box-height ((node-text-func n) n))
                          children
                          (get-color n tree)
                          w
                          h))))

                      (lambda (root x y tree)
                       (or
                        (let find-utterance ((root root))
;                         (if (or (< y (+ (utterance-y root) (utterance-h root))) (null? (utterance-args root)))
                         (if (or (and (= (utterance-w root) (treemap-utterance-total-width root)) (< y (+ (utterance-y root) (utterance-h root))))
                                 (and (= (utterance-h root) (treemap-utterance-total-height root)) (< x (+ (utterance-x root) (utterance-w root))))
                                 (null? (utterance-args root)))
                          root
                          (ormap
                           (lambda (child)
                            (if (and
                                 (>= y (utterance-y child))
                                 (< y (+ (utterance-y child) (treemap-utterance-total-height child)))
                                 (>= x (utterance-x child))
                                 (< x (+ (utterance-x child) (treemap-utterance-total-width child))))
                             (find-utterance child)
                             #f))
                           (utterance-args root))))
                        root))

;                        (with
;                         ((if (or
;                               (above-bottom-of-utterance?)
;                               (utterance-is-closed?)
;                               (has-no-children?)
;                               (is-to-the-right-of-utterance?))
;                           root
;                           (pass-on-to-child)))
;                       
;                         (above-bottom-of-utterance? ()
;                          (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root))))
;                       
;                         (utterance-is-closed? ()
;                          (closed? (utterance-node root) tree))
;                               
;                         (has-no-children? ()
;                          (null? (node-args (utterance-node root))))
;                       
;                         (is-to-the-right-of-utterance? ()
;                          (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
;                       
;                         (pass-on-to-child ()
;                          (ormap
;                           (lambda (child)
;                            (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
;                             (find-utterance child x y tree)
;                             #f))
;                           (utterance-args root))))))

                      (lambda (dir event)
                       (cond
                        ((eq? dir 'up)
                         (set-whole-tree-zoom! Selected-tree (* (/ 1.2) (whole-tree-zoom Selected-tree))))
;                         (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
                        ((eq? dir 'down)
                         (set-whole-tree-zoom! Selected-tree (* 1.2 (whole-tree-zoom Selected-tree))))
;                         (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
                        ((eq? dir 'left)
                         (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
                        ((eq? dir 'right)
                         (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))))))

(define v11ns (list default-v11n treemap-v11n other-v11n other2-v11n))

(define (cycle-v11n event)
 (set-whole-tree-v11n! Selected-tree
  (let* ((cur (whole-tree-v11n Selected-tree))
         (tail (member cur v11ns)))
   (if (or (null? tail) (null? (cdr tail)))
    (car v11ns)
    (cadr tail))))
 (generate-utterance-tree Selected-tree)
 (send Thecanvas on-paint))

(define Trees (list
               (apply whole-tree 
                      (let* ((dummy-n (node '(0 'dummy "dummy" () () () () ()) '() (delay '()) (lambda (_) "t")))
                             (dummy-utterance (utterance dummy-n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
                       (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 0 0 0 0 default-v11n 0 0 1)))
               (apply whole-tree 
                      (let* ((dummy-n (node '(0 'dummy-bar "dummy bar" () () () () ()) '() (delay '()) (lambda (_) "r")))
                             (dummy-utterance (utterance dummy-n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
                       (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 800 30 (- WIDTH 600) 300 other-v11n 0 0 1)))))
(define Selected-tree (cadr Trees))
(define Bar-tree (cadr Trees))

(define (for-all-trees f)
 (for-each f (cdr Trees)))

(define (add-key-evs args)
 (set! key-evs (apply hash-set* key-evs args)))

(define (remove-tree tree)
 (let ((next
       (let ((r (member tree Trees)))
        (if (null? (cdr r))
         (if (null? (cddr Trees))
          (cadr Trees)
          (caddr Trees))
         (cadr r)))))
       (set! Trees (remove tree Trees))
       (if (eq? Selected-tree tree)
        (set! Selected-tree next)
        '())))

(define key-evs
 (with
  ((hash
    #\n new-tree
    #\N replace-major-tree
    #\tab select-new-tree
    #\q (lambda (_) (remove-tree Selected-tree))
    #\h (lambda (_) (go 'left Selected-tree))
    #\j (lambda (_) (go 'down Selected-tree))
    #\k (lambda (_) (go 'up Selected-tree))
    #\l (lambda (_) (go 'right Selected-tree))
    #\A (lambda (_) (semantic-go 'left Selected-tree))
    #\S (lambda (_) (semantic-go 'down Selected-tree))
    #\W (lambda (_) (semantic-go 'up Selected-tree))
    #\D (lambda (_) (semantic-go 'right Selected-tree))
    #\O (lambda (_) (open-u (whole-tree-selection-u Selected-tree) #f Selected-tree) (send Thecanvas on-paint))
    #\c (lambda (_) (close-u (whole-tree-selection-u Selected-tree) #f Selected-tree) (send Thecanvas on-paint))
    #\O (lambda (_) (open-u (whole-tree-selection-u Selected-tree) #t Selected-tree) (send Thecanvas on-paint))
    #\C (lambda (_) (close-u (whole-tree-selection-u Selected-tree) #t Selected-tree) (send Thecanvas on-paint))
    #\z zoom-out
    #\V cycle-v11n
    #\0 (curry nth-child 0)
    #\1 (curry nth-child 1)
    #\2 (curry nth-child 2)
    #\3 (curry nth-child 3)
    #\4 (curry nth-child 4)
    #\5 (curry nth-child 5)
    #\6 (curry nth-child 6)
    #\7 (curry nth-child 7)
    #\8 (curry nth-child 8)
    #\9 (curry nth-child 9)
    'wheel-up (lambda (event)
               (if (or (send event get-control-down) (send event get-shift-down) (send event get-meta-down))
                (begin
                 (if (send event get-control-down)
                  (set! VAR1 (max (min (+ VAR1 VAR1OFFSET) VAR1MAX) VAR1MIN))
                  '())
                 (if (send event get-shift-down)
                  (set! VAR2 (max (min (+ VAR2 VAR2OFFSET) VAR2MAX) VAR2MIN))
                  '())
                 (if (send event get-meta-down)
                  (set! VAR3 (max (min (+ VAR3 VAR3OFFSET) VAR3MAX) VAR3MIN))
                  '())
                 (generate-utterance-tree Selected-tree))
                ((v11n-wheel (whole-tree-v11n Selected-tree)) 'up event))
               (send Thecanvas on-paint))
    'wheel-down (lambda (event)
                 (if (or (send event get-control-down) (send event get-shift-down) (send event get-meta-down))
                  (begin
                   (if (send event get-control-down)
                    (set! VAR1 (max (min (- VAR1 VAR1OFFSET) VAR1MAX) VAR1MIN))
                    '())
                   (if (send event get-shift-down)
                    (set! VAR2 (max (min (- VAR2 VAR2OFFSET) VAR2MAX) VAR2MIN))
                    '())
                   (if (send event get-meta-down)
                    (set! VAR3 (max (min (- VAR3 VAR3OFFSET) VAR3MAX) VAR3MIN))
                    '())
                   (generate-utterance-tree Selected-tree))
                  ((v11n-wheel (whole-tree-v11n Selected-tree)) 'down event))
                 (send Thecanvas on-paint))
    'wheel-left (lambda (event) ((v11n-wheel (whole-tree-v11n Selected-tree)) 'left event) (send Thecanvas on-paint))
    'wheel-right (lambda (event) ((v11n-wheel (whole-tree-v11n Selected-tree)) 'right event) (send Thecanvas on-paint))
    ))

  (nth-child (n event)
   (if (> (length (utterance-args (whole-tree-selection-u Selected-tree))) n)
    (select (list-ref (utterance-args (whole-tree-selection-u Selected-tree)) n) Selected-tree)
    (select (last (utterance-args (whole-tree-selection-u Selected-tree))) Selected-tree))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (new-tree (event)
   (add-to-screen (node-data (utterance-node (whole-tree-selection-u Selected-tree))) (whole-tree-childfunc Selected-tree))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (replace-major-tree (event)
   (let* ((tree (cadr Trees))
          (n (root->node (node-data (utterance-node (whole-tree-selection-u Selected-tree))) (whole-tree-childfunc tree) '())))
    (set-whole-tree-n-tree! tree n)
    (set-whole-tree-utterance-tree! tree ((v11n-node->v11n-utterance (whole-tree-v11n tree)) (whole-tree-n-tree tree) 0 0 0 0 '() tree))
    (set-whole-tree-selection! tree '())
    (set-whole-tree-offset-x! tree 0)
    (set-whole-tree-offset-y! tree 0)
    (set! Selected-tree tree))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (select-new-tree (event)
   (set! Selected-tree
    (if (send event get-shift-down)
     (let ((r (member Selected-tree (reverse Trees))))
      (if (null? (cdddr r))
       (last Trees)
       (cadr r)))
     (let ((r (member Selected-tree Trees)))
      (if (null? (cdr r))
       (if (null? (cdr Trees))
        (car Trees)
        (caddr Trees))
       (cadr r)))))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (zoom-out (event)
   (set-whole-tree-offset-x! Selected-tree (- (utterance-y (whole-tree-selection-u Selected-tree))))
   (set-whole-tree-offset-y! Selected-tree (- (utterance-x (whole-tree-selection-u Selected-tree))))
   (set-whole-tree-zoom! Selected-tree (if (= (whole-tree-zoom Selected-tree) 1) (if VERTICAL (/ (whole-tree-h Selected-tree) (utterance-h (whole-tree-selection-u Selected-tree))) (/ (whole-tree-w Selected-tree) (utterance-w (whole-tree-selection-u Selected-tree) ))) 1))
   (send Thecanvas on-paint))
  ))

(define (go dir tree)
 (let ((new-sel (apply (v11n-find-utterance (whole-tree-v11n tree)) (whole-tree-utterance-tree tree)
                 (cond
                  ((eq? dir 'left)
                   (list (+ (utterance-x (whole-tree-selection-u tree)) -1) (utterance-y (whole-tree-selection-u tree)) tree))
                  ((eq? dir 'down)
                   (list (utterance-x (whole-tree-selection-u tree)) (+ (utterance-y (whole-tree-selection-u tree)) (utterance-h (whole-tree-selection-u tree)) 1) tree))
                  ((eq? dir 'up)
                   (list (utterance-x (whole-tree-selection-u tree)) (+ (utterance-y (whole-tree-selection-u tree)) -1) tree))
                  ((eq? dir 'right)
                   (list (+ (utterance-x (whole-tree-selection-u tree)) (utterance-w (whole-tree-selection-u tree)) 1) (utterance-y (whole-tree-selection-u tree)) tree))))))
 (select (if new-sel new-sel (whole-tree-selection-u tree)) tree))
 (generate-utterance-tree tree)
 (send Thecanvas on-paint))

(define (semantic-go dir tree)
 (let ((new-sel (cond
                 ((eq? dir 'left)
                  (let ((mem (takef (utterance-args (utterance-parent (whole-tree-selection-u tree) tree)) (negate (curry equal? (whole-tree-selection-u tree))))))
                   (if (null? mem)
                    #f
                    (last mem))))
                 ((eq? dir 'down)
                  (car (utterance-args (whole-tree-selection-u tree))))
                 ((eq? dir 'up)
                  (utterance-parent (whole-tree-selection-u tree) tree))
                 ((eq? dir 'right)
                  (let ((mem (member (whole-tree-selection-u tree) (utterance-args (utterance-parent (whole-tree-selection-u tree) tree)))))
                   (if (or (null? mem) (null? (cdr mem)))
                    #f
                    (cadr mem)))))))
  (select (or new-sel (whole-tree-selection-u tree)) tree)
  (generate-utterance-tree tree)
  (send Thecanvas on-paint)))

(define mouse-evs
 (hash
  'dragging (lambda (event)
             (define-mouse-handler (rel-x rel-y)
              (cond
               ((send event get-left-down)
                (set-whole-tree-offset-x! Chosen-tree (+ (whole-tree-offset-x Chosen-tree) (/ (+ (- (car Mouse-pos)) rel-x) (whole-tree-zoom tree))))
                (set-whole-tree-offset-y! Chosen-tree (+ (whole-tree-offset-y Chosen-tree) (/ (+ (- (cdr Mouse-pos)) rel-y) (whole-tree-zoom tree))))
                (set! Mouse-pos (cons rel-x rel-y))
                (send Thecanvas on-paint)))))
  'motion (lambda (event)
           (define-mouse-handler (clicked)
            (let ((text (format "~s" (node-data (utterance-node clicked)))))
             (if (equal? Info text)
              '()
              (begin
               (set! Info text)
               (paint-info Info #t))))))
  'left-down (lambda (event)
              (define-mouse-handler (rel-x rel-y)
               (set! Chosen-tree tree)
               (set! Mouse-pos (cons rel-x rel-y))))
  'left-up (lambda (event)
            (define-mouse-handler (clicked)
             (select clicked tree)
             (generate-utterance-tree tree)
             (send Thecanvas on-paint)))
  'middle-down (lambda (event)
                (define-mouse-handler (clicked)
                 (select clicked tree)
                 (close-u (whole-tree-selection-u tree) (send event get-control-down) Selected-tree)
                 (send Thecanvas on-paint)))
  'right-down (lambda (event)
               (define-mouse-handler (clicked)
                (select clicked tree)
                (open-u (whole-tree-selection-u tree) (send event get-control-down) Selected-tree)
                (send Thecanvas on-paint)))
))

(define (open-u u deep? tree)
 (set-whole-tree-open! tree (set-union (whole-tree-open tree)
                             (list->set
                              (map
                               node-laddr
                               (if deep?
                                (flatten (cons (utterance-node u) (map (lambda (a) (node-deep-args a (compose (curryr member '(scoped var)) cadr node-data))) (node-args (utterance-node u)))))
                                (letrec
                                 ((lam (lambda (l)
                                        (if (or (null? l) (ormap (lambda (x) (closed? x tree)) l))
                                         l
                                         (lam (flatten (map node-args l)))))))
                                 (lam (list (utterance-node u)))))))))
  (generate-utterance-tree tree))

(define (close-u u deep? tree)
 (set-whole-tree-open! tree (set-subtract (whole-tree-open tree)
                             (list->set
                              (map
                               node-laddr
                               (if deep?
                                (let ((remnant (if (closed? (utterance-node u) tree) (utterance-parent u tree) u)))
                                 (select remnant tree)
                                 (flatten (node-deep-args (utterance-node remnant) (curryr closed? tree))))
                                (if (closed? (utterance-node u) tree)
                                 (begin
                                  (select (utterance-parent u tree) tree)
                                  (flatten (node-deep-args (utterance-node (utterance-parent u tree)) (curryr closed? tree))))
                                 (letrec
                                  ((lam (lambda (l)
                                         (if (andmap
                                              (lambda (x) (or (closed? x tree) (null? (node-args x))))
                                              (flatten (map node-args l)))
                                          (append l (flatten (map node-args l)))
                                          (lam (flatten (map node-args l)))))))
                                  (lam (list (utterance-node u))))))))))
 (generate-utterance-tree tree))

(define (node-deep-args n pred)
 (if (pred n) '() (cons n (map (lambda (a) (node-deep-args a pred)) (node-args n)))))

(define my-canvas%
 (with
  ((class* canvas% ()
   (inherit with-gl-context swap-gl-buffers)
 
   (define/override (on-paint)
;     (with
      (with-gl-context
        (lambda ()
         (gl-clear-color 0.0 0.0 0.0 0.0)
         (gl-clear 'color-buffer-bit)

         (paint-info Info #f)
         (paint-bar (whole-tree-selection-u Selected-tree))
   
         (gl-enable 'scissor-test)
   
         (for-each (lambda (tree) ((v11n-paint-tree (whole-tree-v11n tree)) tree)) Trees)
   
         (gl-disable 'scissor-test)
         (swap-gl-buffers))))
  
;      (paint-tree (tree)
;       (with
;        ((apply gl-scissor (whole-tree-dim tree))
;         (apply gl-viewport (whole-tree-dim tree))
;         (gl-matrix-mode 'projection)
;         (gl-load-identity)
;         (gl-ortho
;          (- (whole-tree-offset-x tree))
;          (+ (- (whole-tree-offset-x tree)) (/ (whole-tree-w tree) (whole-tree-zoom tree)))
;          (+ (whole-tree-offset-y tree) (- (/ (whole-tree-h tree) (whole-tree-zoom tree))))
;          (whole-tree-offset-y tree)
;          -1.0
;          1.0)
;         (gl-matrix-mode 'modelview)
;         (gl-load-identity)
;         (utterance-paint (whole-tree-utterance-tree tree) tree))
;  
;        (utterance-paint (u tree)
;         (with
;          ((let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
;                  (x (utterance-x u))
;                  (y (utterance-y u))
;                  (w (utterance-w u))
;                  (h (utterance-h u))
;                  (text-w (utterance-text-w u))
;                  (text-h (utterance-text-h u))
;                  (args (utterance-args u))
;                  (clr (utterance-clr u)))
;            (if (invisible? x y w h tree)
;             '()
;             (begin
;              (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
;              (if (< (whole-tree-zoom tree) 1) '()
;               (draw-text
;                text
;                (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
;                (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
;                (car clr)
;                tree))
;              (for-each (lambda (arg) (utterance-paint arg tree)) args)))))
;  
;         (invisible? (x y w h tree)
;          (or
;           (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
;           (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
;           (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
;           (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))
;    
;         (center (offset lenwhole lenpiece start width)
;          (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
;           (if (< visible-width lenpiece)
;            (if (< offset start)
;             (- (+ offset lenwhole) lenpiece)
;             offset)
;            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))))))))

   (define/override (on-event event)
    (if (send event dragging?)
      ((hash-ref mouse-evs 'dragging) event)
      (if (hash-has-key? mouse-evs (send event get-event-type))
        ((hash-ref mouse-evs (send event get-event-type)) event)
        '())))

   (define/override (on-char event)
    (cond
     (INSERTMODE ((hash-ref key-evs 'insert) event))
     (SCOPEMODE ((hash-ref key-evs 'scope) event))
     (ARGIFYMODE ((hash-ref key-evs 'argify) event))
     (SEARCHMODE ((hash-ref key-evs 'search) event))
     ((hash-has-key? key-evs (send event get-key-code))
      ((hash-ref key-evs (send event get-key-code)) event))
     (#t '())))

   (super-instantiate () (style '(gl)))

   (with-gl-context
    (lambda ()
     (initialize-font)))))))

(define (draw-rectangle clr x y w h)
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))

 (gl-begin 'quads)
 (gl-vertex x (- y))
 (gl-vertex (+ x w) (- y))
 (gl-vertex (+ x w) (- (+ y h)))
 (gl-vertex x (- (+ y h)))
 (gl-end))

(define (draw-text text x y clr (tree '()))
; (gl-enable 'blend)
; (gl-enable 'texture-2d)
; (gl-disable 'depth-test)
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))
; (if (null? tree)
;  (begin
;   (glBitmap 0 0 0 0 x y (make-cvector _uint8 1)))
;  (begin
;   (gl-raster-pos (- 1 (whole-tree-offset-x tree)) (- (whole-tree-offset-y tree) 1))
;   (glBitmap 0 0 0 0 (+ x (* (whole-tree-zoom tree) (whole-tree-offset-x tree))) (- (+ y (* (whole-tree-zoom tree) (whole-tree-offset-y tree)))) (make-cvector _uint8 1))))
 (gl-push-matrix)
 (gl-translate x (- y) 0)
; (gl-rotate 45 0 0 1)
 (ftglRenderFont Font text 65535)
 (gl-pop-matrix))

(define INSERTMODE #f)

(define (enter-insert-mode) (set! INSERTMODE #t) (set! Search-tree (add-to-screen (list 0 'list '() '() '() '() '() '()) (whole-tree-childfunc Selected-tree))))

(define (exit-insert-mode) (set! INSERTMODE #f))

(define SCOPEMODE #f)

(define (enter-scope-mode) (set! SCOPEMODE #t))

(define (exit-scope-mode) (set! SCOPEMODE #f))

(define ARGIFYMODE #f)

(define (enter-argify-mode) (set! ARGIFYMODE #t))

(define (exit-argify-mode) (set! ARGIFYMODE #f))

(define SEARCHMODE #f)

(define (enter-search-mode) (set! SEARCHMODE #t) (set! Search-tree (add-to-screen (list 0 'list '() '() '() '() '() '()) (whole-tree-childfunc Selected-tree))))

(define (exit-search-mode) (set! SEARCHMODE #f))

(define (paint-bar u)
 (with
  ((gl-enable 'scissor-test)
   (apply gl-scissor (rel->gl Bar-dim))
   (gl-viewport 0 30 WIDTH 300)
   (gl-matrix-mode 'projection)
   (gl-load-identity)
   (gl-ortho 0 WIDTH 30 330 -1.0 1.0)
   (gl-clear 'color-buffer-bit)
   (paint-u)
   (gl-disable 'scissor-test))

  (paint-u ()
   (gl-color (/ (car (car INFOCOLOR)) 255) (/ (cadr (car INFOCOLOR)) 255) (/ (caddr (car INFOCOLOR)) 255))
   (paint paint-name 0 310)
   (paint paint-id 0 290)
   (paint paint-type 0 270)
   (paint paint-open 0 250)
   (paint paint-laddr 0 230)
   (paint-utterance-data 0 210)
   (paint paint-lexical-parent 0 110)
   (paint paint-free-variables 0 90)
   (paint paint-bound-variables 0 70)
   (paint paint-open-set 0 50)
   (paint paint-vars 0 30)
   (paint-neighborhood)
   (paint-runtime-vals)
   (set-tree)
   (paint-search-results))

  (paint (func x y)
   (gl-push-matrix)
   (gl-translate x y 0)
   (func)
   (gl-pop-matrix))

  (paint-name ()
   (ftglRenderFont Font ((node-text-func (utterance-node u)) (utterance-node u)) 65535))

  (paint-id ()
   (ftglRenderFont Font (format "id:  ~a" (car (node-data (utterance-node u)))) 65535))

  (paint-type ()
   (ftglRenderFont Font (format "type:  ~a" (cadr (node-data (utterance-node u)))) 65535))

  (paint-open ()
   (ftglRenderFont Font (if (open? (utterance-node u) Selected-tree) "open" "closed") 65535))

  (paint-laddr ()
   (ftglRenderFont Font (format "laddr:  ~a" (node-laddr (utterance-node u))) 65535))

  (paint-utterance-data (x y)
   (gl-push-matrix)
   (gl-translate x y 0)
   (ftglRenderFont Font (format "x:  ~a" (utterance-x u)) 65535)
   (gl-translate 100 0 0)
   (ftglRenderFont Font (format "y:  ~a" (utterance-y u)) 65535)
   (gl-translate -100 -20 0)
   (ftglRenderFont Font (format "w:  ~a" (utterance-w u)) 65535)
   (gl-translate 100 0 0)
   (ftglRenderFont Font (format "h:  ~a" (utterance-h u)) 65535)
   (gl-translate -100 -20 0)
   (ftglRenderFont Font (format "text-w:  ~a" (utterance-text-w u)) 65535)
   (gl-translate 100 0 0)
   (ftglRenderFont Font (format "text-h:  ~a" (utterance-text-h u)) 65535)
   (gl-translate -100 -20 0)
   (ftglRenderFont Font (format "color:  ~a" (utterance-clr u)) 65535)
   (gl-translate 0 -20 0)
   (ftglRenderFont Font (format "children:  ~a" (map (lambda (u) ((node-text-func (utterance-node u)) (utterance-node u))) (utterance-args u))) 65535)
   (gl-pop-matrix))

  (paint-lexical-parent ()
   (ftglRenderFont Font (format "lexcial parent:  ~a" (cadddr (cdddr (node-data (utterance-node u))))) 65535))

  (paint-free-variables ()
   (ftglRenderFont Font (format "free variables:  ~a" (cadddr (cdr (node-data (utterance-node u))))) 65535))

  (paint-bound-variables ()
   (ftglRenderFont Font (format "bound variables:  ~a" (cadddr (cddr (node-data (utterance-node u))))) 65535))

  (paint-open-set ()
   (ftglRenderFont Font (format "open set:  ~a" (whole-tree-open Selected-tree)) 65535))

  (paint-vars ()
   (ftglRenderFont Font (format "VAR1:  ~a VAR2:  ~a VAR3:  ~a" VAR1 VAR2 VAR3) 65535))

  (paint-neighborhood ()
   (gl-push-matrix)
   (gl-translate 300 310 0)
   (map paint-triple (cadddr (node-data (utterance-node u))) (build-list (length (cadddr (node-data (utterance-node u)))) identity))
   (gl-pop-matrix))

  (paint-triple (t n)
   (gl-translate 0 -20 0)
   (ftglRenderFont Font (format "~a" t) 65535))

  (paint-runtime-vals ()
   (gl-push-matrix)
   (gl-translate 600 313 0)
   (let ((width (apply max (map (lambda (u) (box-width (format "~a" ((node-text-func (utterance-node u)) (utterance-node u))))) (cons u (utterance-args u))))))
    (for-each
     (lambda (u n)
      (let ((text (format "~a" ((node-text-func (utterance-node u)) (utterance-node u)))))
       (draw-rectangle '(96 96 0) (if (zero? n) 600 610) (+ -330 (* 20 n)) (if (zero? n) (+ width 10) width) 20)
       (gl-color 1 1 1)
       (gl-translate 0 -20 0)
       (ftglRenderFont Font text 65535)))
     (cons u (utterance-args u))
     (build-list (+ 1 (length (utterance-args u))) identity))
    (gl-pop-matrix)
    (gl-push-matrix)
    (gl-translate (+ 610 width) 0 0)
    (let ((r-vs (list-ref (node-data (utterance-node u)) 7)))
     (if (null? r-vs)
      '()
      (for-each
       (lambda (r-v n)
        (gl-translate -40 0 0)
        (ftglRenderFont Font (format "~a" (cadr r-v)) 65535)
;        (gl-translate 0 -20 0)
        (for-each
         (lambda (node-id val)
          (for-each
           (lambda (i-u m)
            (if (eq? (car (node-data (utterance-node i-u))) node-id)
             (begin
              (gl-raster-pos 0 -20)
              (ftglRenderFont Font (format "~a" val) 65535))
             '()))
           (utterance-args u)
           (build-list (length (utterance-args u)) identity)))
         (list-ref r-v 3)
         (list-ref r-v 4)))
       r-vs
       (build-list (length r-vs) identity))))))

  (set-tree ()
   (set-whole-tree-childfunc! Bar-tree (whole-tree-childfunc Selected-tree))
   (set-whole-tree-n-tree! Bar-tree (root->node (node-data (utterance-node u)) (whole-tree-childfunc Bar-tree) '()))
   (set-whole-tree-open! Bar-tree (set))
   (set-whole-tree-selection! Bar-tree '())
   (set-whole-tree-offset-x! Bar-tree 0)
   (set-whole-tree-offset-y! Bar-tree 0)
   (set-whole-tree-zoom! Bar-tree 1)
   (generate-utterance-tree Bar-tree)
   (open-u (whole-tree-utterance-tree Bar-tree) #t Bar-tree))

  (paint-search-results ()
   (gl-push-matrix)
   (gl-translate (- WIDTH 300) 310 0)
   (map paint-triple Search-results (build-list (length Search-results) identity))
   (gl-pop-matrix))))

(define Search-results '())
(define Search-tree '())

(define (set-search-results res) (set! Search-results res))

(define (scroll-search-results) (if (null? Search-results) '() (set! Search-results (append (cdr Search-results) (list (car Search-results))))))

(define (show-search-tree get-rep)
 (if (null? Search-results)
  '()
  (begin
   (set-whole-tree-childfunc! Search-tree (whole-tree-childfunc Selected-tree))
   (set-whole-tree-n-tree! Search-tree (root->node (get-rep (caar Search-results)) (whole-tree-childfunc Search-tree) '()))
   (set-whole-tree-open! Search-tree (set))
   (set-whole-tree-selection! Search-tree '())
   (set-whole-tree-offset-x! Search-tree 0)
   (set-whole-tree-offset-y! Search-tree 0)
   (set-whole-tree-zoom! Search-tree 1)
   (generate-utterance-tree Search-tree)
   (open-u (whole-tree-utterance-tree Search-tree) #t Search-tree))))

(define (remove-search-tree)
 (remove-tree Search-tree))

(define (set-info text)
 (set! Info text))

(define (paint-info text swap)
 (gl-enable 'scissor-test)
 (apply gl-scissor (rel->gl Info-dim))
 (gl-viewport 0 0 WIDTH 30)
 (gl-matrix-mode 'projection)
 (gl-load-identity)
 (gl-ortho 0 WIDTH 0 30 -1.0 1.0)
 (gl-clear 'color-buffer-bit)
 (gl-color (/ (car (car INFOCOLOR)) 255) (/ (cadr (car INFOCOLOR)) 255) (/ (caddr (car INFOCOLOR)) 255))
 (gl-raster-pos 0 10)
 (gl-push-matrix)
 (gl-translate 0 5 0)
 (ftglRenderFont Font (substring text 0 (min (string-length text) 200)) 65535)
 (gl-pop-matrix)
 (if swap
  (send Thecanvas swap-gl-buffers)
  '())
 (gl-disable 'scissor-test))


(define (whole-tree-dim tree)
 (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

(define (in? dim x y)
 (and (> x (car dim)) (> y (cadr dim)) (< x (+ (car dim) (caddr dim))) (< y (+ (cadr dim) (cadddr dim)))))

(define (utterance-parent u tree)
 (let loop ((root (whole-tree-utterance-tree tree)))
  (if (member u (utterance-args root))
   root
   (ormap loop (utterance-args root)))))
 
; (apply (v11n-find-utterance (whole-tree-v11n tree)) (whole-tree-utterance-tree tree)
;  (if VERTICAL
;   (list (+ (utterance-x u) -1) (utterance-y u) tree)
;   (list (utterance-x u) (+ (utterance-y u) -1) tree))))
   
;(define (find-utterance root x y tree)
; (with
;  ((if (or
;        (above-bottom-of-utterance?)
;        (utterance-is-closed?)
;        (has-no-children?)
;        (is-to-the-right-of-utterance?))
;    root
;    (pass-on-to-child)))
;
;  (above-bottom-of-utterance? ()
;   (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root))))
;
;  (utterance-is-closed? ()
;   (closed? (utterance-node root) tree))
;        
;  (has-no-children? ()
;   (null? (node-args (utterance-node root))))
;
;  (is-to-the-right-of-utterance? ()
;   (>= (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
;
;  (pass-on-to-child ()
;   (ormap
;    (lambda (child)
;     (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
;      (find-utterance child x y tree)
;      #f))
;    (utterance-args root)))))

(define (select u tree)
 (set! Selected-tree tree)
 (set-whole-tree-selection! tree (node-laddr (utterance-node u)))
 (let ((x (+ (whole-tree-offset-x tree) (utterance-x u)))
       (y (+ (whole-tree-offset-y tree) (utterance-y u)))
       (w (utterance-w u))
       (h (utterance-h u)))
  (if
   (or
    (and (negative? (+ x w)) (not VERTICAL))
    (> x (/ (whole-tree-w tree) (whole-tree-zoom tree))))
   (let ((c (+ (utterance-x u) (/ w 2))))
    (set-whole-tree-offset-x! tree (- (+ c (- (/ (whole-tree-w tree) (whole-tree-zoom tree) 2))))))
   '())
  (if
   (or
    (and (negative? (+ y h)) VERTICAL)
    (> y (/ (whole-tree-h tree) (whole-tree-zoom tree))))
   (let ((c (+ (utterance-y u) (/ h 2))))
    (set-whole-tree-offset-y! tree (- (+ c (- (/ (whole-tree-h tree) (whole-tree-zoom tree) 2))))))
   '())))

(define Info-dim (list 0 (- HEIGHT 30) WIDTH 30))
(define Bar-dim (list 0 (- HEIGHT 330) WIDTH 300))

(define Font #f)

(define (initialize-font)
 (set! Font (ftglCreateTextureFont "/home/philip/olddesktop/vilisp/VeraMono.ttf"))
 (ftglSetFontFaceSize Font 12 72))

(define PADDING 5)

(define (box-width box)
 (+ PADDING (ftglGetFontAdvance Font box)))

(define (box-height box)
 (ftglGetFontLineHeight Font))

(define (box-maj-dim box)
 (if VERTICAL (box-height box) (box-width box)))

(define (node-width n tree)
 (if VERTICAL (box-width ((node-text-func (node-data n)) n)) (node-maj-dim n tree)))

(define (node-height n tree)
 (if VERTICAL (node-maj-dim n tree) CELLHEIGHT))

(define (node-maj-dim n tree)
 (if (closed? n tree)
  (box-maj-dim ((node-text-func (node-data n)) n))
  (max
   (box-maj-dim ((node-text-func (node-data n)) n))
   (foldl
    +
    0
    (map (lambda (arg) (node-maj-dim arg tree)) (node-args n))))))

(define VERTICAL #f)
(define Mouse-pos (cons -1 -1))
(define Info "test")
(define SCROLLDIST 100)
(define Thecanvas (new my-canvas% (parent win)))
;(send Thecanvas on-paint)

(define (utterance-maj-dim u) (if VERTICAL (utterance-y u) (utterance-x u)))
(define (utterance-maj-dim-span u) (if VERTICAL (utterance-h u) (utterance-w u)))
(define (utterance-min-dim u) (if VERTICAL (utterance-x u) (utterance-y u)))
(define (utterance-min-dim-span u) (if VERTICAL (utterance-w u) (utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))

(define (rel->gl l)
 (list (car l) (- HEIGHT (+ (cadddr l) (cadr l))) (caddr l) (cadddr l)))

(define (generate-utterance-tree tree)
 (set-whole-tree-utterance-tree! tree ((v11n-node->v11n-utterance (whole-tree-v11n tree)) (whole-tree-n-tree tree) (utterance-x (whole-tree-utterance-tree tree)) (utterance-y (whole-tree-utterance-tree tree)) (if VERTICAL (node-width (whole-tree-n-tree tree) tree) -1) 0 1 tree)))

(define (find-utterance-from-laddr tree laddr)
 (if (null? laddr)
  tree
  (find-utterance-from-laddr (list-ref (utterance-args tree) (car laddr)) (cdr laddr))))

(define (find-utterance-from-laddr-safe tree laddr)
 (if (null? laddr)
  tree
  (if (> (length (utterance-args tree)) (car laddr))
   (find-utterance-from-laddr-safe (list-ref (utterance-args tree) (car laddr)) (cdr laddr))
   #f)))

(define (update-childfuncs childfunc)
 (map (curryr set-whole-tree-childfunc! childfunc) (cdr Trees))
 (map (lambda (t) (set-whole-tree-n-tree! t (root->node (node-data (whole-tree-n-tree t)) childfunc (node-laddr (whole-tree-n-tree t))))) (cdr Trees))
 (map generate-utterance-tree (cdr Trees))
 (send Thecanvas on-paint))

(define (add-to-screen root childfunc)
 (let ((tree (display-on-screen 0 0 (/ WIDTH 2) 0 root childfunc)))
  (normalize-trees)
  tree))

(define (normalize-trees)
 (let* ((num (length (cdddr Trees)))
        (h (round (/ (- HEIGHT 330) num))))
  (for-each
   (lambda (tree n)
    (set-whole-tree-x! tree (/ WIDTH 2))
    (set-whole-tree-y! tree (+ 330 (* (- (+ -1 num) n) h)))
    (set-whole-tree-w! tree (/ WIDTH 2))
    (set-whole-tree-h! tree h))
   (cdddr Trees)
   (build-list num identity))))

(define (display-on-screen x y w h root childfunc)
 (let ((tree
  (let* ((n (root->node root childfunc '()))
         (dummy-utterance (utterance n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
   (whole-tree
    n
    childfunc
    dummy-utterance
    (set)
    '()
    x
    y
    w
    h
    default-v11n
    0
    0
    1))))
 (set-whole-tree-utterance-tree! tree ((v11n-node->v11n-utterance default-v11n) (whole-tree-n-tree tree) 0 0 w 0 '() tree))
 (set-whole-tree-selection! tree '())
 (set! Trees (append Trees (list tree)))
 tree))

;(define get-color (lambda (a94 a95) (letrec ((v35 a95)(v26 a94)) (if (equal? (node-laddr v26) (whole-tree-selection v35)) SELCOLOR (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) SEL2COLOR (letrec ((v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26))))) (if (odd? (length (node-laddr v26))) (if (zero? v85) COLOR5 (if (odd? v85) COLOR1 COLOR2)) (if (zero? v85) COLOR6 (if (odd? v85) COLOR3 COLOR4)))))))))

;(define get-color (lambda (a94 a95) (letrec ((v35 a95) (v26 a94) (v96 (cons (quote (255 255 255)) (quote (0 0 255))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (255 255 255)) (quote (112 0 112))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26)))) (v188 (cons (quote (255 255 255)) (quote (80 0 0))))) (if (odd? (length (node-laddr v26))) (if (zero? v85) v188 (letrec ((v146 (cons (quote (255 255 255)) (quote (96 32 0)))) (v144 (lambda () -)) (v128 (cons (quote (255 255 255)) (quote (96 0 0))))) (if (odd? v85) v128 v146))) (if (zero? v85) v188 (letrec ((v160 (cons (quote (255 255 255)) (quote (96 72 0)))) (v174 (cons (quote (255 255 255)) (quote (96 96 0))))) (if (odd? v85) v160 v174)))))))))))

;(define get-color (lambda (a94 a95) (letrec ((v206 (lambda (a222 a229 a230) (letrec ((v221 a222) (v223 (letrec ((v227 a230) (v228 a229)) (* v227 v228))) (v231 (* v223 (- 1 (abs (- (modulo (* v221 6) 2) 1)))))) (cond ((< v221 (/ 1 6)) (list v223 v231 0)) ((< v221 (/ 2 6)) (list v231 v223 0)) ((< v221 (/ 3 6)) (list 0 v223 v231)) ((< v221 (/ 4 6)) (list 0 v231 v223)) ((< v221 (/ 5 6)) (list v231 0 v223)) ((< v221 (/ 6 6)) (list v223 0 v231)))))) (v35 a95) (v26 a94) (v96 (cons (quote (255 255 255)) (map (curry * 255) (v206 VAR1 1.0 1.0))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (255 255 255)) (quote (112 0 112))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26)))) (v188 (cons (quote (255 255 255)) (quote (80 0 0))))) (if (odd? (length (node-laddr v26))) (if (zero? v85) v188 (letrec ((v146 (cons (quote (255 255 255)) (quote (96 32 0)))) (v144 (lambda () -)) (v128 (cons (quote (255 255 255)) (quote (96 0 0))))) (if (odd? v85) v128 v146))) (if (zero? v85) v188 (letrec ((v160 (cons (quote (255 255 255)) (quote (96 72 0)))) (v174 (cons (quote (255 255 255)) (quote (96 96 0))))) (if (odd? v85) v160 v174)))))))))))

(define get-color (lambda (a94 a95) (letrec ((v432 0.6180339887498949) (v354 (lambda (a359 a364) (letrec ((v358 a359)) (- v358 (letrec ((v363 a364)) (* v363 (truncate (/ v358 v363)))))))) (v206 (lambda (a222 a229 a230) (letrec ((v221 a222) (v223 (letrec ((v227 a230) (v228 a229)) (* v227 v228))) (v231 (* v223 (- 1 (abs (- (v354 (* v221 6) 2) 1)))))) (cond ((< v221 (/ 1 6)) (list v223 v231 0)) ((< v221 (/ 2 6)) (list v231 v223 0)) ((< v221 (/ 3 6)) (list 0 v223 v231)) ((< v221 (/ 4 6)) (list 0 v231 v223)) ((< v221 (/ 5 6)) (list v231 0 v223)) ((< v221 (/ 6 6)) (list v223 0 v231)))))) (v35 a95) (v26 a94) (v96 (cons (quote (0 0 0)) (map (curry * 255) (v206 0.15 1.0 1.0))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (0 0 0)) (map (curry * 255) (v206 0.15 0.9 0.9))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v450 (lambda () (v354 (* (letrec ((v452 (lambda (a462) (letrec ((v461 a462)) (if (null? v461) 0 (last v461)))))) (v452 (node-laddr v26))) v432) 1))) (v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26)))) (v188 (cons (quote (255 255 255)) (quote (80 0 0)))) (v373 (cons (quote (255 255 255)) (map (curry * 255) (v206 (v450) 0.8 0.8)))) (v405 (cons (quote (255 255 255)) (map (curry * 255) (v206 (v450) 0.6 0.8))))) (if (odd? (length (node-laddr v26))) v373 v405))))))))

(define h (make-hash))

;(define get-color (lambda (a94 a95) (set! h (make-hash)) (letrec ((v35 a95) (v26 a94) (v96 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 98 (cons res (hash-ref! h 98 (quote ())))) res) (let ((res (quote (0 0 255)))) (hash-set! h 104 (cons res (hash-ref! h 104 (quote ())))) res)))) (hash-set! h 5 (cons res (hash-ref! h 5 (quote ())))) res))) (let ((res (if (let ((res (equal? (let ((res (node-laddr v26))) (hash-set! h 8 (cons res (hash-ref! h 8 (quote ())))) res) (let ((res (whole-tree-selection v35))) (hash-set! h 9 (cons res (hash-ref! h 9 (quote ())))) res)))) (hash-set! h 4 (cons res (hash-ref! h 4 (quote ())))) res) v96 (letrec ((v110 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 112 (cons res (hash-ref! h 112 (quote ())))) res) (let ((res (quote (112 0 112)))) (hash-set! h 122 (cons res (hash-ref! h 122 (quote ())))) res)))) (hash-set! h 16 (cons res (hash-ref! h 16 (quote ())))) res))) (let ((res (if (let ((res (equal? (let ((res (car (let ((res (node-data v26))) (hash-set! h 23 (cons res (hash-ref! h 23 (quote ())))) res)))) (hash-set! h 19 (cons res (hash-ref! h 19 (quote ())))) res) (let ((res (car (let ((res (node-data (let ((res (utterance-node (let ((res (whole-tree-selection-u v35))) (hash-set! h 32 (cons res (hash-ref! h 32 (quote ())))) res)))) (hash-set! h 30 (cons res (hash-ref! h 30 (quote ())))) res)))) (hash-set! h 28 (cons res (hash-ref! h 28 (quote ())))) res)))) (hash-set! h 20 (cons res (hash-ref! h 20 (quote ())))) res)))) (hash-set! h 15 (cons res (hash-ref! h 15 (quote ())))) res) v110 (letrec ((v85 (let ((res (if (let ((res (null? (let ((res (node-laddr v26))) (hash-set! h 70 (cons res (hash-ref! h 70 (quote ())))) res)))) (hash-set! h 66 (cons res (hash-ref! h 66 (quote ())))) res) 0 (let ((res (last (let ((res (node-laddr v26))) (hash-set! h 74 (cons res (hash-ref! h 74 (quote ())))) res)))) (hash-set! h 68 (cons res (hash-ref! h 68 (quote ())))) res)))) (hash-set! h 64 (cons res (hash-ref! h 64 (quote ())))) res)) (v188 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 190 (cons res (hash-ref! h 190 (quote ())))) res) (let ((res (quote (80 0 0)))) (hash-set! h 196 (cons res (hash-ref! h 196 (quote ())))) res)))) (hash-set! h 57 (cons res (hash-ref! h 57 (quote ())))) res))) (let ((res (if (let ((res (odd? (let ((res (length (let ((res (node-laddr v26))) (hash-set! h 52 (cons res (hash-ref! h 52 (quote ())))) res)))) (hash-set! h 50 (cons res (hash-ref! h 50 (quote ())))) res)))) (hash-set! h 46 (cons res (hash-ref! h 46 (quote ())))) res) (let ((res (if (let ((res (zero? v85))) (hash-set! h 56 (cons res (hash-ref! h 56 (quote ())))) res) v188 (letrec ((v146 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 148 (cons res (hash-ref! h 148 (quote ())))) res) (let ((res (quote (96 32 0)))) (hash-set! h 154 (cons res (hash-ref! h 154 (quote ())))) res)))) (hash-set! h 145 (cons res (hash-ref! h 145 (quote ())))) res)) (v144 (lambda () -)) (v128 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 130 (cons res (hash-ref! h 130 (quote ())))) res) (let ((res (quote (96 0 0)))) (hash-set! h 136 (cons res (hash-ref! h 136 (quote ())))) res)))) (hash-set! h 81 (cons res (hash-ref! h 81 (quote ())))) res))) (let ((res (if (let ((res (odd? v85))) (hash-set! h 80 (cons res (hash-ref! h 80 (quote ())))) res) v128 v146))) (hash-set! h 58 (cons res (hash-ref! h 58 (quote ())))) res))))) (hash-set! h 47 (cons res (hash-ref! h 47 (quote ())))) res) (let ((res (if (let ((res (zero? v85))) (hash-set! h 60 (cons res (hash-ref! h 60 (quote ())))) res) v188 (letrec ((v160 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 162 (cons res (hash-ref! h 162 (quote ())))) res) (let ((res (quote (96 72 0)))) (hash-set! h 168 (cons res (hash-ref! h 168 (quote ())))) res)))) (hash-set! h 90 (cons res (hash-ref! h 90 (quote ())))) res)) (v174 (let ((res (cons (let ((res (quote (255 255 255)))) (hash-set! h 176 (cons res (hash-ref! h 176 (quote ())))) res) (let ((res (quote (96 96 0)))) (hash-set! h 182 (cons res (hash-ref! h 182 (quote ())))) res)))) (hash-set! h 91 (cons res (hash-ref! h 91 (quote ())))) res))) (let ((res (if (let ((res (odd? v85))) (hash-set! h 89 (cons res (hash-ref! h 89 (quote ())))) res) v160 v174))) (hash-set! h 62 (cons res (hash-ref! h 62 (quote ())))) res))))) (hash-set! h 48 (cons res (hash-ref! h 48 (quote ())))) res)))) (hash-set! h 17 (cons res (hash-ref! h 17 (quote ())))) res))))) (hash-set! h 6 (cons res (hash-ref! h 6 (quote ())))) res))))) (hash-set! h 1 (cons res (hash-ref! h 1 (quote ())))) (display h) (newline) res))))


;(define get-color
;  (lambda (a94 a95)
;    (letrec ((v35 a95) (v26 a94) (v96 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 100 (cons res (hash-ref! h 100 (quote ())))) res)))) (hash-set! h 98 (cons res (hash-ref! h 98 (quote ())))) res) (let ((res (quote (let ((res (0 0 255))) (hash-set! h 106 (cons res (hash-ref! h 106 (quote ())))) res)))) (hash-set! h 104 (cons res (hash-ref! h 104 (quote ())))) res)))) (hash-set! h 5 (cons res (hash-ref! h 5 (quote ())))) res)))
;      (let ((res
;              (if (let ((res (equal? (let ((res (node-laddr v26))) (hash-set! h 8 (cons res (hash-ref! h 8 (quote ())))) res) (let ((res (whole-tree-selection v35))) (hash-set! h 9 (cons res (hash-ref! h 9 (quote ())))) res)))) (hash-set! h 4 (cons res (hash-ref! h 4 (quote ())))) res)
;                v96
;                (letrec ((v110 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 114 (cons res (hash-ref! h 114 (quote ())))) res)))) (hash-set! h 112 (cons res (hash-ref! h 112 (quote ())))) res) (let ((res (quote (let ((res (112 0 112))) (hash-set! h 124 (cons res (hash-ref! h 124 (quote ())))) res)))) (hash-set! h 122 (cons res (hash-ref! h 122 (quote ())))) res)))) (hash-set! h 16 (cons res (hash-ref! h 16 (quote ())))) res)))
;                  (let ((res (if (let ((res (equal? (let ((res (car (let ((res (node-data v26))) (hash-set! h 23 (cons res (hash-ref! h 23 (quote ())))) res)))) (hash-set! h 19 (cons res (hash-ref! h 19 (quote ())))) res) (let ((res (car (let ((res (node-data (let ((res (utterance-node (let ((res (whole-tree-selection-u v35))) (hash-set! h 32 (cons res (hash-ref! h 32 (quote ())))) res)))) (hash-set! h 30 (cons res (hash-ref! h 30 (quote ())))) res)))) (hash-set! h 28 (cons res (hash-ref! h 28 (quote ())))) res)))) (hash-set! h 20 (cons res (hash-ref! h 20 (quote ())))) res))))
;                                   (hash-set! h 15 (cons res (hash-ref! h 15 (quote ()))))
;                                   res)
;                               v110
;                               (letrec ((v85 (let ((res (if (let ((res (null? (let ((res (node-laddr v26))) (hash-set! h 70 (cons res (hash-ref! h 70 (quote ())))) res)))) (hash-set! h 66 (cons res (hash-ref! h 66 (quote ())))) res) 0 (let ((res (last (let ((res (node-laddr v26))) (hash-set! h 74 (cons res (hash-ref! h 74 (quote ())))) res)))) (hash-set! h 68 (cons res (hash-ref! h 68 (quote ())))) res))))
;                                               (hash-set! h 64 (cons res (hash-ref! h 64 (quote ()))))
;                                               res))
;                                        (v188 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 192 (cons res (hash-ref! h 192 (quote ())))) res)))) (hash-set! h 190 (cons res (hash-ref! h 190 (quote ())))) res) (let ((res (quote (let ((res (80 0 0))) (hash-set! h 198 (cons res (hash-ref! h 198 (quote ())))) res)))) (hash-set! h 196 (cons res (hash-ref! h 196 (quote ())))) res))))
;                                                (hash-set! h 57 (cons res (hash-ref! h 57 (quote ()))))
;                                                res)))
;                                 (let ((res (if (let ((res (odd? (let ((res (length (let ((res (node-laddr v26))) (hash-set! h 52 (cons res (hash-ref! h 52 (quote ())))) res)))) (hash-set! h 50 (cons res (hash-ref! h 50 (quote ())))) res))))
;                                                  (hash-set! h 46 (cons res (hash-ref! h 46 (quote ()))))
;                                                  res)
;                                              (let ((res (if (let ((res (zero? v85)))
;                                                               (hash-set! h 56 (cons res (hash-ref! h 56 (quote ()))))
;                                                               res)
;                                                           v188
;                                                           (letrec ((v146 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 150 (cons res (hash-ref! h 150 (quote ())))) res)))) (hash-set! h 148 (cons res (hash-ref! h 148 (quote ())))) res) (let ((res (quote (let ((res (96 32 0))) (hash-set! h 156 (cons res (hash-ref! h 156 (quote ())))) res)))) (hash-set! h 154 (cons res (hash-ref! h 154 (quote ())))) res)))) (hash-set! h 145 (cons res (hash-ref! h 145 (quote ())))) res)) (v144 (lambda () -)) (v128 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 132 (cons res (hash-ref! h 132 (quote ())))) res)))) (hash-set! h 130 (cons res (hash-ref! h 130 (quote ())))) res) (let ((res (quote (let ((res (96 0 0))) (hash-set! h 138 (cons res (hash-ref! h 138 (quote ())))) res)))) (hash-set! h 136 (cons res (hash-ref! h 136 (quote ())))) res)))) (hash-set! h 81 (cons res (hash-ref! h 81 (quote ())))) res)))
;                                                             (let ((res (if (let ((res (odd? v85))) (hash-set! h 80 (cons res (hash-ref! h 80 (quote ())))) res) v128 v146))) (hash-set! h 58 (cons res (hash-ref! h 58 (quote ())))) res)))))
;                                                (hash-set! h 47 (cons res (hash-ref! h 47 (quote ())))) res)
;                                              (let ((res (if (let ((res (zero? v85))) (hash-set! h 60 (cons res (hash-ref! h 60 (quote ())))) res) v188 (letrec ((v160 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 164 (cons res (hash-ref! h 164 (quote ())))) res)))) (hash-set! h 162 (cons res (hash-ref! h 162 (quote ())))) res) (let ((res (quote (let ((res (96 72 0))) (hash-set! h 170 (cons res (hash-ref! h 170 (quote ())))) res)))) (hash-set! h 168 (cons res (hash-ref! h 168 (quote ())))) res)))) (hash-set! h 90 (cons res (hash-ref! h 90 (quote ())))) res)) (v174 (let ((res (cons (let ((res (quote (let ((res (255 255 255))) (hash-set! h 178 (cons res (hash-ref! h 178 (quote ())))) res)))) (hash-set! h 176 (cons res (hash-ref! h 176 (quote ())))) res) (let ((res (quote (let ((res (96 96 0))) (hash-set! h 184 (cons res (hash-ref! h 184 (quote ())))) res)))) (hash-set! h 182 (cons res (hash-ref! h 182 (quote ())))) res)))) (hash-set! h 91 (cons res (hash-ref! h 91 (quote ())))) res))) (let ((res (if (let ((res (odd? v85))) (hash-set! h 89 (cons res (hash-ref! h 89 (quote ())))) res) v160 v174))) (hash-set! h 62 (cons res (hash-ref! h 62 (quote ())))) res)))))
;                                                (hash-set! h 48 (cons res (hash-ref! h 48 (quote ()))))
;                                                res))))
;                                   (hash-set! h 17 (cons res (hash-ref! h 17 (quote ()))))
;                                   res)))))
;                    (hash-set! h 6 (cons res (hash-ref! h 6 (quote ()))))
;                    res)))))
;        (hash-set! h 1 (cons res (hash-ref! h 1 (quote ()))))
;        res))))


;(define get-color (lambda (a94 a95) (letrec ((v35 a95)(v26 a94) (v96 (cons (quote (255 255 255)) (quote (0 0 255))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (255 255 255)) (quote (112 0 112))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26))))(v188 (cons (quote (255 255 255)) (quote (80 0 0))))) (if (odd? (length (node-laddr v26))) (if (zero? v85) v188 (letrec ((v146 (cons (quote (255 255 255)) (quote (96 32 0))))(v144 (lambda () -))(v128 (cons (quote (255 255 255)) (quote (96 0 0))))) (if (odd? v85) v128 v146))) (if (zero? v85) v188 (letrec ((v160 (cons (quote (255 255 255)) (quote (96 72 0))))(v174 (cons (quote (255 255 255)) (quote (96 96 0))))) (if (odd? v85) v160 v174)))))))))))


;(define get-color (lambda (a94 a95) (letrec ((v35 a95)(v26 a94)(v96 (cons (quote (255 255 255)) (quote (0 0 255))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (255 255 255)) (quote (112 0 112))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26))))(v188 (cons (quote (255 255 255)) (quote (80 0 0))))) (if (odd? (length (node-laddr v26))) (if (zero? v85) v188 (letrec ((v146 (cons (quote (255 255 255)) (quote (96 32 0))))(v144 (lambda () -))(v128 (cons (quote (255 255 255)) (quote (96 0 0))))(v142 v2)) (if (odd? v85) v128 v146))) (if (zero? v85) v188 (letrec ((v160 (cons (quote (255 255 255)) (quote (96 72 0))))(v174 (cons (quote (255 255 255)) (quote (96 96 0))))) (if (odd? v85) v160 v174)))))))))))


;(define (node->utterance n x y w row siblings tree)
; (with
;  ((let ((children 
;          (if (closed? n tree)
;           '()
;           (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (node-width arg tree)) (node-args n))) -1)))
;            (caddr
;             (foldl
;              (lambda (arg data)
;               (let ((res (node->utterance
;                           arg
;                           (if VERTICAL (+ (car data) w) (car data))
;                           (if VERTICAL (cadr data) (+ (cadr data) (node-height arg tree)))
;                           child-w
;                           (+ 1 row)
;                           (- (length (node-args n)) 1)
;                           tree)))
;                (list
;                 (if VERTICAL (car data) (+ (car data) (utterance-w res)))
;                 (if VERTICAL (+ (cadr data) (node-height arg tree)) (cadr data))
;                 (if (null? res)
;                  (caddr data)
;                  (append
;                   (caddr data)
;                   (list res))))))
;              (list x y '())
;              (node-args n)))))))
;    (utterance
;     n
;     x
;     y
;     (if VERTICAL w (max (box-width ((node-text-func n) n)) (apply + (map utterance-w children))))
;     (node-height n tree)
;     (box-width ((node-text-func n) n))
;     (box-height ((node-text-func n) n))
;     children
;     (get-color n tree))))))

;  (get-color (n siblings tree)
;   (if (equal? (node-laddr n) (whole-tree-selection tree))
;    SELCOLOR 
;    (if (equal? (car (node-data n)) (car (node-data (utterance-node (whole-tree-selection-u tree)))))
;     SEL2COLOR ; (cons '(255 255 255) (list VAR1 VAR2 VAR3))
;     (if (eq? COLORSCHEME 'gradient)
;      (let* ((pos (if (null? (node-laddr n)) 0 (last (node-laddr n))))
;   	  (diff (/ pos (if (zero? siblings) 1 siblings)))
;   	  (col (map + INITIALCOLOR (map round (map * (make-list 3 diff) COLORRANGES)))))
;       (if #f ;(null? (ess-man-args (node-man n)))
;        CODECOLOR1
;        (cons (apply make-object color% FGCOLOR) (apply make-object color% col))))
;      (let* ((row (length (node-laddr n)))
;   	  (col (if (null? (node-laddr n)) 0 (last (node-laddr n)))))
;       (if #f ;(null? (ess-man-args (node-man n)))
;        (if (zero? col) CODECOLOR3 (if (odd? col) CODECOLOR1 CODECOLOR2))
;        (if (odd? row)
;         (if (zero? col) COLOR5 (if (odd? col) COLOR1 COLOR2))
;         (if (zero? col) COLOR6 (if (odd? col) COLOR3 COLOR4)))))))))

(define (root->node data childlist laddr)
 (node data laddr
  (delay
   (let ((children (childlist data)))
    (map
     (lambda (arg n) (root->node arg childlist (append laddr (list n))))
     children
     (build-list (length children) values))))
  (lambda (n) (format "~s" (caddr (node-data n))))))

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(define Chosen-tree '())
(define-for-syntax mouse-handler-hash (hash
			'clicked '((v11n-find-utterance (whole-tree-v11n tree))
                                   (whole-tree-utterance-tree tree)
                                   (+
                                    (- (whole-tree-x tree))
                                    (- (/ (send event get-x) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
                                   (+
                                    (+ (- HEIGHT) (whole-tree-h tree) (whole-tree-y tree))
                                    (- (/ (send event get-y) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))
                                   tree)
			'abs-x '(- (/ (send event get-x) (whole-tree-zoom tree)) (whole-tree-offset-x tree))
			'abs-y '(- (/ (send event get-y) (whole-tree-zoom tree)) (whole-tree-offset-y tree))
			'rel-x '(send event get-x)
			'rel-y '(send event get-y)))

(define-syntax (define-mouse-handler stx)
 (let* ((args (syntax->datum stx))
	(reqs (cadr args))
	(code (cddr args))
	(vals (map (lambda (req) (list req (hash-ref mouse-handler-hash req))) reqs)))
  (datum->syntax stx `(let ((tree (let ((x (send event get-x)) (y (send event get-y))) (or (findf (compose (curryr in? x (- HEIGHT y)) whole-tree-dim) Trees) Selected-tree))))
                       (let ,vals ,@code)))))


(send win show #t)
