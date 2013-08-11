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
(define-ftgl ftglSetFontFaceSize (_fun _FTGLfont _int _int -> _void))
(define-ftgl ftglGetFontLineHeight (_fun _FTGLfont -> _float))
(define-ftgl ftglGetFontAdvance (_fun _FTGLfont _string -> _float))
(define-ftgl ftglRenderFont (_fun _FTGLfont _string _int -> _void))
(define-ftgl ftglDestroyFont (_fun _FTGLfont -> _void))

(provide my-canvas% box-width box-height box-maj-dim node-width node-height node-maj-dim VERTICAL display-on-screen Thecanvas Info Selected-tree utterance-parent utterance-node utterance-args node-data node-laddr whole-tree-selection-u whole-tree-selection set-whole-tree-selection! whole-tree-open set-whole-tree-open! whole-tree-utterance-tree add-key-evs key-evs update-childfuncs set-info enter-insert-mode exit-insert-mode enter-link-mode exit-link-mode enter-var-mode exit-var-mode paint-info go find-utterance-from-laddr-safe)

(struct node (data laddr prom-args text-func) #:transparent)
(struct utterance (node x y w h text-w text-h args clr) #:transparent)
(struct whole-tree (n-tree childfunc utterance-tree open selection x y w h offset-x offset-y zoom) #:mutable)

(define (whole-tree-selection-u tree)
 (find-utterance-from-laddr-safe (whole-tree-utterance-tree tree) (whole-tree-selection tree)))

(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define Trees (list
                (apply whole-tree 
                       (let* ((dummy-n (node '(0 'dummy "dummy" '()) '() (delay '()) (lambda (_) "t")))
                              (dummy-utterance (utterance dummy-n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
                        (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 0 0 0 0 0 0 1)))
                (apply whole-tree 
                       (let* ((dummy-n (node '(0 'dummy "dummy" '()) '() (delay '()) (lambda (_) "t")))
                              (dummy-utterance (utterance dummy-n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
                        (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 600 30 (- WIDTH 600) 300 0 0 1)))))
(define Selected-tree (car Trees))
(define Bar-tree (cadr Trees))

(define COLORSCHEME 'alternate)
(define COLOR1 (cons '(255 255 255) '(0 0 127)))
(define COLOR2 (cons '(255 255 255) '(63 0 127)))
(define COLOR3 (cons '(255 255 255) '(0 63 127)))
(define COLOR4 (cons '(255 255 255) '(63 63 127)))
(define COLOR5 (cons '(255 255 255) '(0 0 159)))
(define COLOR6 (cons '(255 255 255) '(0 0 159)))
(define CODECOLOR1 (cons '(255 255 255) '(255 0 0)))
(define CODECOLOR2 (cons '(255 255 255) '(223 0 0)))
(define CODECOLOR3 (cons '(255 255 255) '(191 0 0)))
(define SELCOLOR (cons '(128 0 128) '(0 191 0)))
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

(define (add-key-evs args)
 (set! key-evs (apply hash-set* key-evs args)))

(define key-evs
 (with
  ((hash
    #\n new-tree
    #\N replace-major-tree
    #\tab select-new-tree
    #\q remove-tree
    #\h (lambda (_) (go 'left Selected-tree))
    #\j (lambda (_) (go 'down Selected-tree))
    #\k (lambda (_) (go 'up Selected-tree))
    #\l (lambda (_) (go 'right Selected-tree))
    #\o (lambda (_) (open-u (whole-tree-selection-u Selected-tree) #f Selected-tree) (send Thecanvas on-paint))
    #\c (lambda (_) (close-u (whole-tree-selection-u Selected-tree) #f Selected-tree) (send Thecanvas on-paint))
    #\O (lambda (_) (open-u (whole-tree-selection-u Selected-tree) #t Selected-tree) (send Thecanvas on-paint))
    #\C (lambda (_) (close-u (whole-tree-selection-u Selected-tree) #t Selected-tree) (send Thecanvas on-paint))
    #\z zoom-out
    'wheel-up (lambda (_) (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))) (send Thecanvas on-paint))
    'wheel-down (lambda (_) (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))) (send Thecanvas on-paint))
    'wheel-left (lambda (_) (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))) (send Thecanvas on-paint))
    'wheel-right (lambda (_) (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))) (send Thecanvas on-paint))
    ))

  (new-tree (event)
   (add-to-screen (node-data (utterance-node (whole-tree-selection-u Selected-tree))) (whole-tree-childfunc Selected-tree))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (replace-major-tree (event)
   (let* ((tree (cadr Trees))
          (n (root->node (node-data (utterance-node (whole-tree-selection-u Selected-tree))) (whole-tree-childfunc tree) '())))
    (set-whole-tree-n-tree! tree n)
    (set-whole-tree-utterance-tree! tree (node->utterance (whole-tree-n-tree tree) 0 0 0 0 '() tree))
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

  (remove-tree (event)
   (let ((next
         (let ((r (member Selected-tree Trees)))
          (if (null? (cdr r))
           (if (null? (cdr Trees))
            (car Trees)
            (cadr Trees))
           (cadr r)))))
         (set! Trees (remove Selected-tree Trees))
         (set! Selected-tree next)))

  (zoom-out (event)
   (set-whole-tree-offset-x! Selected-tree (- (utterance-y (whole-tree-selection-u Selected-tree))))
   (set-whole-tree-offset-y! Selected-tree (- (utterance-x (whole-tree-selection-u Selected-tree))))
   (set-whole-tree-zoom! Selected-tree (if (= (whole-tree-zoom Selected-tree) 1) (if VERTICAL (/ (whole-tree-h Selected-tree) (utterance-h (whole-tree-selection-u Selected-tree))) (/ (whole-tree-w Selected-tree) (utterance-w (whole-tree-selection-u Selected-tree) ))) 1))
   (send Thecanvas on-paint))
  ))

(define (go dir tree)
 (let ((new-sel (apply find-utterance (whole-tree-utterance-tree tree)
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
                                (flatten (cons (utterance-node u) (map (lambda (a) (node-deep-args a (compose (curry eq? 'scoped) cadr node-data))) (node-args (utterance-node u)))))
;                                                         (lambda (n) #f
                                ;(null? (ess-man-args (node-man n)))
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
                                          l
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
     (with
      ((with-gl-context
        (lambda ()
         (gl-clear-color 0.0 0.0 0.0 0.0)
         (gl-clear 'color-buffer-bit)

         (paint-info Info #f)
         (paint-bar (whole-tree-selection-u Selected-tree))
   
         (gl-enable 'scissor-test)
   
         (for-each paint-tree Trees)
   
         (gl-disable 'scissor-test)
         (swap-gl-buffers))))
  
      (paint-tree (tree)
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
                (* (whole-tree-zoom tree) (center x w text-w (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
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
            (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))
    
         (draw-rectangle (clr x y w h)
          (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))
        
          (gl-begin 'quads)
          (gl-vertex x (- y))
          (gl-vertex (+ x w) (- y))
          (gl-vertex (+ x w) (- (+ y h)))
          (gl-vertex x (- (+ y h)))
          (gl-end))
    
         (draw-text (text x y clr tree)
          (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))
          (gl-raster-pos (- 1 (whole-tree-offset-x tree)) (- (whole-tree-offset-y tree) 1))
          (glBitmap 0 0 0 0 (+ x (whole-tree-offset-x tree)) (- (+ y (whole-tree-offset-y tree))) (make-cvector _uint8 1))
          (ftglRenderFont Font text 65535))))))))

   (define/override (on-event event)
    (if (send event dragging?)
      ((hash-ref mouse-evs 'dragging) event)
      (if (hash-has-key? mouse-evs (send event get-event-type))
        ((hash-ref mouse-evs (send event get-event-type)) event)
        '())))

   (define/override (on-char event)
    (cond
     (INSERTMODE ((hash-ref key-evs 'insert) event))
     (LINKMODE ((hash-ref key-evs 'link) event))
     (VARMODE ((hash-ref key-evs 'var) event))
     ((hash-has-key? key-evs (send event get-key-code))
      ((hash-ref key-evs (send event get-key-code)) event))
     (#t '())))
  
   (super-instantiate () (style '(gl)))))
  ))

(define INSERTMODE #f)

(define (enter-insert-mode) (set! INSERTMODE #t))

(define (exit-insert-mode) (set! INSERTMODE #f))

(define LINKMODE #f)

(define (enter-link-mode) (set! LINKMODE #t))

(define (exit-link-mode) (set! LINKMODE #f))

(define VARMODE #f)

(define (enter-var-mode) (set! VARMODE #t))

(define (exit-var-mode) (set! VARMODE #f))

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
   (gl-raster-pos 0 310)
   (paint-name)
   (gl-raster-pos 0 290)
   (paint-id)
   (gl-raster-pos 0 270)
   (paint-type)
   (gl-raster-pos 0 250)
   (paint-open)
   (gl-raster-pos 0 230)
   (paint-laddr)
   (gl-raster-pos 0 210)
   (paint-utterance-data)
   (gl-raster-pos 0 110)
   (paint-neighborhood)
   (set-tree))

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

  (paint-utterance-data ()
   (ftglRenderFont Font (format "x:  ~a" (utterance-x u)) 65535)
   (gl-raster-pos 100 210)
   (ftglRenderFont Font (format "y:  ~a" (utterance-y u)) 65535)
   (gl-raster-pos 0 190)
   (ftglRenderFont Font (format "w:  ~a" (utterance-w u)) 65535)
   (gl-raster-pos 100 190)
   (ftglRenderFont Font (format "h:  ~a" (utterance-h u)) 65535)
   (gl-raster-pos 0 170)
   (ftglRenderFont Font (format "text-w:  ~a" (utterance-text-w u)) 65535)
   (gl-raster-pos 100 170)
   (ftglRenderFont Font (format "text-h:  ~a" (utterance-text-h u)) 65535)
   (gl-raster-pos 0 150)
   (ftglRenderFont Font (format "color:  ~a" (utterance-clr u)) 65535)
   (gl-raster-pos 0 130)
   (ftglRenderFont Font (format "children:  ~a" (map (lambda (u) ((node-text-func (utterance-node u)) (utterance-node u))) (utterance-args u))) 65535))

  (paint-neighborhood ()
   (map paint-triple (cadddr (node-data (utterance-node u))) (build-list (length (cadddr (node-data (utterance-node u)))) identity)))

  (paint-triple (t n)
   (gl-raster-pos 300 (- 310 (* 20 n)))
   (ftglRenderFont Font (format "~a" t) 65535))

  (set-tree ()
   (set-whole-tree-childfunc! Bar-tree (whole-tree-childfunc Selected-tree))
   (set-whole-tree-n-tree! Bar-tree (root->node (node-data (utterance-node u)) (whole-tree-childfunc Bar-tree) '()))
   (set-whole-tree-open! Bar-tree (set))
   (set-whole-tree-selection! Bar-tree '())
   (set-whole-tree-offset-x! Bar-tree 0)
   (set-whole-tree-offset-y! Bar-tree 0)
   (set-whole-tree-zoom! Bar-tree 1)
   (generate-utterance-tree Bar-tree)
   (open-u (whole-tree-utterance-tree Bar-tree) #t Bar-tree))))

(define (set-info text)
 (set! Info text))

(define (paint-info text swap)
 (gl-enable 'scissor-test)
 (apply gl-scissor (rel->gl Info-dim))
; (apply gl-viewport (rel->gl Info-dim))
 (gl-viewport 0 0 WIDTH 30)
 (gl-matrix-mode 'projection)
 (gl-load-identity)
 (gl-ortho 0 WIDTH 0 30 -1.0 1.0)
 (gl-clear 'color-buffer-bit)
 (gl-color (/ (car (car INFOCOLOR)) 255) (/ (cadr (car INFOCOLOR)) 255) (/ (caddr (car INFOCOLOR)) 255))
 (gl-raster-pos 0 10)
 (ftglRenderFont Font (substring text 0 (min (string-length text) 200)) 65535)
 (if swap
  (send Thecanvas swap-gl-buffers)
  '())
 (gl-disable 'scissor-test))


(define (whole-tree-dim tree)
 (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

(define (in? dim x y)
 (and (> x (car dim)) (> y (cadr dim)) (< x (+ (car dim) (caddr dim))) (< y (+ (cadr dim) (cadddr dim)))))

(define (utterance-parent u tree)
 (apply find-utterance (whole-tree-utterance-tree tree)
  (if VERTICAL
   (list (+ (utterance-x u) -1) (utterance-y u) tree)
   (list (utterance-x u) (+ (utterance-y u) -1) tree))))
   
(define (find-utterance root x y tree)
 (if (or
      (< (min-dim x y) (+ (utterance-min-dim root) (utterance-min-dim-span root)))
      (closed? (utterance-node root) tree)
      (null? (node-args (utterance-node root)))
      (> (maj-dim x y) (let ((baby (last (utterance-args root)))) (+ (utterance-maj-dim baby) (utterance-maj-dim-span baby)))))
  root
  (ormap
   (lambda (child)
    (if (< (maj-dim x y) (+ (utterance-maj-dim child) (utterance-maj-dim-span child)))
     (find-utterance child x y tree)
     #f))
   (utterance-args root))))

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

(define Font (ftglCreateBitmapFont "/home/philip/olddesktop/vilisp/VeraMono.ttf"))
(ftglSetFontFaceSize Font 12 72)

(define (box-width box)
 (ftglGetFontAdvance Font box))

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

(define (utterance-maj-dim u) (if VERTICAL (utterance-y u) (utterance-x u)))
(define (utterance-maj-dim-span u) (if VERTICAL (utterance-h u) (utterance-w u)))
(define (utterance-min-dim u) (if VERTICAL (utterance-x u) (utterance-y u)))
(define (utterance-min-dim-span u) (if VERTICAL (utterance-w u) (utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))

(define (rel->gl l)
 (list (car l) (- HEIGHT (+ (cadddr l) (cadr l))) (caddr l) (cadddr l)))

(define (generate-utterance-tree tree)
 (set-whole-tree-utterance-tree! tree (node->utterance (whole-tree-n-tree tree) (utterance-x (whole-tree-utterance-tree tree)) (utterance-y (whole-tree-utterance-tree tree)) (if VERTICAL (node-width (whole-tree-n-tree tree) tree) -1) 0 1 tree)))
; (set-whole-tree-selection-u! tree (find-utterance-from-laddr (whole-tree-utterance-tree tree) (node-laddr (utterance-node (whole-tree-selection-u tree))))))

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
 (display-on-screen 0 0 (/ WIDTH 2) 0 root childfunc)
 (normalize-trees))

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
    0
    0
    1))))
 (set-whole-tree-utterance-tree! tree (node->utterance (whole-tree-n-tree tree) 0 0 w 0 '() tree))
 (set-whole-tree-selection! tree '())
 (set! Trees (append Trees (list tree)))))

(define (node->utterance n x y w row siblings tree)
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
     (get-color n siblings tree))))

  (get-color (n siblings tree)
   (if (equal? (node-laddr n) (whole-tree-selection tree))
    SELCOLOR
    (if (eq? COLORSCHEME 'gradient)
     (let* ((pos (if (null? (node-laddr n)) 0 (last (node-laddr n))))
  	  (diff (/ pos (if (zero? siblings) 1 siblings)))
  	  (col (map + INITIALCOLOR (map round (map * (make-list 3 diff) COLORRANGES)))))
      (if #f ;(null? (ess-man-args (node-man n)))
       CODECOLOR1
       (cons (apply make-object color% FGCOLOR) (apply make-object color% col))))
     (let* ((row (length (node-laddr n)))
  	  (col (if (null? (node-laddr n)) 0 (last (node-laddr n)))))
      (if #f ;(null? (ess-man-args (node-man n)))
       (if (zero? col) CODECOLOR3 (if (odd? col) CODECOLOR1 CODECOLOR2))
       (if (odd? row)
        (if (zero? col) COLOR5 (if (odd? col) COLOR1 COLOR2))
        (if (zero? col) COLOR6 (if (odd? col) COLOR3 COLOR4))))))))
  ))

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
			'clicked '(find-utterance
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
