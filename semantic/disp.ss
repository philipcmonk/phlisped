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

(provide my-canvas% box-width box-height box-maj-dim ess-addr-width ess-addr-height ess-addr-maj-dim VERTICAL ess-man-text display-on-screen Thecanvas Info)

(struct ess-addr (man laddr prom-args))
(struct ess-utterance (addr x y w h text-w text-h args clr))
(struct whole-tree (addr-tree childfunc utterance-tree open selection x y w h offset-x offset-y zoom) #:mutable)

(define Trees (list (apply whole-tree 
  (let* ((dummy-addr (ess-addr '(0 . "") '() (delay '())))
         (dummy-utterance (ess-utterance dummy-addr 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
   (list dummy-addr (lambda (a) '()) dummy-utterance (set) dummy-utterance 0 0 0 0 0 0 1)))))
(define Selected-tree (car Trees))

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

(define WIDTH (* 1 1600))
(define HEIGHT 899)

(define win (new frame% (label "vilisp") (min-width WIDTH) (min-height HEIGHT)))

(define (open? addr tree) (set-member? (whole-tree-open tree) addr))
(define closed? (negate open?))
(define ess-addr-args (compose force ess-addr-prom-args))

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
   
         (gl-enable 'scissor-test)
   
         (for-each paint-tree Trees)
   
         (gl-disable 'scissor-test)
         (paint-info Info #f)
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
         (ess-utterance-paint (whole-tree-utterance-tree tree) tree))
  
        (ess-utterance-paint (u tree)
         (with
          ((let* ((text (ess-man-text (ess-addr-man (ess-utterance-addr u))))
                  (x (ess-utterance-x u))
                  (y (ess-utterance-y u))
                  (w (ess-utterance-w u))
                  (h (ess-utterance-h u))
                  (text-w (ess-utterance-text-w u))
                  (text-h (ess-utterance-text-h u))
                  (args (ess-utterance-args u))
                  (clr (ess-utterance-clr u)))
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
              (for-each (lambda (arg) (ess-utterance-paint arg tree)) args)))))
  
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
    (cond
     ((send event dragging?)
      (define-mouse-handler (rel-x rel-y)
       (cond
        ((send event get-left-down)
         (set-whole-tree-offset-x! Chosen-tree (+ (whole-tree-offset-x Chosen-tree) (/ (+ (- (car Mouse-pos)) rel-x) (whole-tree-zoom tree))))
         (set-whole-tree-offset-y! Chosen-tree (+ (whole-tree-offset-y Chosen-tree) (/ (+ (- (cdr Mouse-pos)) rel-y) (whole-tree-zoom tree))))
 	(set! Mouse-pos (cons rel-x rel-y))
 	(send this on-paint)))))
     ((eq? (send event get-event-type) 'motion)
      (define-mouse-handler (clicked)
       (let ((text (format "~s" (ess-addr-man (ess-utterance-addr clicked)))))
        (if (equal? Info text)
         '()
         (begin
          (set! Info text)
          (paint-info Info #t))))))
     ((eq? (send event get-event-type) 'left-down)
      (define-mouse-handler (rel-x rel-y)
       (set! Chosen-tree tree)
       (set! Mouse-pos (cons rel-x rel-y))))
     ((eq? (send event get-event-type) 'left-up)
      (define-mouse-handler (clicked)
       (select clicked tree)
       (generate-utterance-tree tree)
       (send this on-paint)))
     ((eq? (send event get-event-type) 'middle-down)
      (define-mouse-handler (clicked)
       (select clicked tree)
       (close-u (whole-tree-selection tree) (send event get-control-down) Selected-tree)))
     ((eq? (send event get-event-type) 'right-down)
      (define-mouse-handler (clicked)
       (select clicked tree)
       (open-u (whole-tree-selection tree) (send event get-control-down) Selected-tree)))
     (#t '())))

   (define/override (on-char event)
    (cond
     ((eq? (send event get-key-code) #\f)
      (begin (set! VERTICAL (not VERTICAL)) (generate-utterance-tree Selected-tree) (send this on-paint)))
     ((eq? (send event get-key-code) #\F)
      (begin (set! COLORSCHEME (if (eq? COLORSCHEME 'gradient) 'alternate 'gradient)) (generate-utterance-tree Selected-tree) (send this on-paint)))
     ((eq? (send event get-key-code) #\n)
      (add-to-screen (ess-addr-man (ess-utterance-addr (whole-tree-selection Selected-tree))) (whole-tree-childfunc Selected-tree))
      (generate-utterance-tree Selected-tree)
      (send this on-paint))
     ((eq? (send event get-key-code) #\N)
      (let* ((tree (cadr Trees))
             (addr (root->ess-addr (ess-addr-man (ess-utterance-addr (whole-tree-selection Selected-tree))) (whole-tree-childfunc tree) '())))
       (set-whole-tree-addr-tree! tree addr)
       (set-whole-tree-utterance-tree! tree (ess-addr->ess-utterance (whole-tree-addr-tree tree) 0 0 0 0 '() tree))
       (set-whole-tree-selection! tree (whole-tree-utterance-tree tree))
       (set-whole-tree-offset-x! tree 0)
       (set-whole-tree-offset-y! tree 0)
       (set! Selected-tree tree))
      (generate-utterance-tree Selected-tree)
      (send this on-paint))
     ((eq? (send event get-key-code) #\tab)
      (set! Selected-tree
       (if (send event get-shift-down)
        (let ((r (member Selected-tree (reverse Trees))))
         (if (null? (cddr r))
          (last Trees)
          (cadr r)))
        (let ((r (member Selected-tree Trees)))
         (if (null? (cdr r))
          (if (null? (cdr Trees))
           (car Trees)
           (cadr Trees))
          (cadr r)))))
      (generate-utterance-tree Selected-tree)
      (send this on-paint))
     ((eq? (send event get-key-code) #\q)
      (let ((next
       (let ((r (member Selected-tree Trees)))
        (if (null? (cdr r))
         (if (null? (cdr Trees))
          (car Trees)
          (cadr Trees))
         (cadr r)))))
       (set! Trees (remove Selected-tree Trees))
       (set! Selected-tree next))
      (normalize-trees) ;***
      (generate-utterance-tree Selected-tree)
      (send this on-paint))
     ((eq? (send event get-key-code) #\h)
      (go 'left Selected-tree))
     ((eq? (send event get-key-code) #\j)
      (go 'down Selected-tree))
     ((eq? (send event get-key-code) #\k)
      (go 'up Selected-tree))
     ((eq? (send event get-key-code) #\l)
      (go 'right Selected-tree))
     ((eq? (send event get-key-code) #\o)
      (open-u (whole-tree-selection Selected-tree) #f Selected-tree))
     ((eq? (send event get-key-code) #\c)
      (close-u (whole-tree-selection Selected-tree) #f Selected-tree))
     ((eq? (send event get-key-code) #\O)
      (open-u (whole-tree-selection Selected-tree) #t Selected-tree))
     ((eq? (send event get-key-code) #\C)
      (close-u (whole-tree-selection Selected-tree) #t Selected-tree))
     ((eq? (send event get-key-code) #\z)
      (set-whole-tree-offset-x! Selected-tree (- (ess-utterance-y (whole-tree-selection Selected-tree))))
      (set-whole-tree-offset-y! Selected-tree (- (ess-utterance-x (whole-tree-selection Selected-tree))))
      (set-whole-tree-zoom! Selected-tree (if (= (whole-tree-zoom Selected-tree) 1) (if VERTICAL (/ (whole-tree-h Selected-tree) (ess-utterance-h (whole-tree-selection Selected-tree))) (/ (whole-tree-w Selected-tree) (ess-utterance-w (whole-tree-selection Selected-tree) ))) 1))
      (send this on-paint))
     ((eq? (send event get-key-code) 'wheel-up)
      (if VERTICAL
       (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree)))
       (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree))))
      (send this on-paint))
     ((eq? (send event get-key-code) 'wheel-down)
      (if VERTICAL
       (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree)))
       (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree))))
      (send this on-paint))
     ((eq? (send event get-key-code) 'wheel-left)
      (if VERTICAL
       (set-whole-tree-offset-x! Selected-tree (+ SCROLLDIST (whole-tree-offset-x Selected-tree)))
       (set-whole-tree-offset-y! Selected-tree (+ SCROLLDIST (whole-tree-offset-y Selected-tree))))
      (send this on-paint))
     ((eq? (send event get-key-code) 'wheel-right)
      (if VERTICAL
       (set-whole-tree-offset-x! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-x Selected-tree)))
       (set-whole-tree-offset-y! Selected-tree (+ (- SCROLLDIST) (whole-tree-offset-y Selected-tree))))
      (send this on-paint))))
  
   (super-instantiate () (style '(gl)))))

  (go (dir tree)
   (let ((new-sel (apply find-utterance (whole-tree-utterance-tree tree)
  		 (cond
  		  ((eq? dir 'left)
  		   (list (+ (ess-utterance-x (whole-tree-selection tree)) -1) (ess-utterance-y (whole-tree-selection tree)) tree))
  		  ((eq? dir 'down)
  		   (list (ess-utterance-x (whole-tree-selection tree)) (+ (ess-utterance-y (whole-tree-selection tree)) (ess-utterance-h (whole-tree-selection tree)) 1) tree))
  		  ((eq? dir 'up)
  		   (list (ess-utterance-x (whole-tree-selection tree)) (+ (ess-utterance-y (whole-tree-selection tree)) -1) tree))
  		  ((eq? dir 'right)
  		   (list (+ (ess-utterance-x (whole-tree-selection tree)) (ess-utterance-w (whole-tree-selection tree)) 1) (ess-utterance-y (whole-tree-selection tree)) tree))))))
   (select (if new-sel new-sel (whole-tree-selection tree)) tree))
   (generate-utterance-tree tree)
   (send Thecanvas on-paint))

  (open-u (u deep? tree)
   (set-whole-tree-open! tree (set-union (whole-tree-open tree) (list->set
  			     (if deep?
  			      (flatten (ess-addr-deep-args (ess-utterance-addr u) (lambda (addr) #f
                                ;(null? (ess-man-args (ess-addr-man addr)))
                                )))
  			      (letrec
  			       ((lam (lambda (l)
  				      (if (or (null? l) (ormap (lambda (x) (closed? x tree)) l))
  				       l
  				       (lam (flatten (map ess-addr-args l)))))))
  			       (lam (list (ess-utterance-addr u))))))))
    (generate-utterance-tree tree)
    (send Thecanvas on-paint))

   (close-u (u deep? tree)
    (set-whole-tree-open! tree (set-subtract (whole-tree-open tree) (list->set
   				(if deep?
   				 (let ((remnant (if (closed? (ess-utterance-addr u) tree) (ess-utterance-parent u tree) u)))
   				  (select remnant tree)
   				  (flatten (ess-addr-deep-args (ess-utterance-addr remnant) (curryr closed? tree))))
   				 (if (closed? (ess-utterance-addr u) tree)
   				  (begin
   				   (select (ess-utterance-parent u tree) tree)
   				   (flatten (ess-addr-deep-args (ess-utterance-addr (ess-utterance-parent u tree)) (curryr closed? tree))))
   				  (letrec
   				   ((lam (lambda (l)
   					  (if (andmap
   					       (lambda (x) (or (closed? x tree) (null? (ess-addr-args x))))
   					       (flatten (map ess-addr-args l)))
   					   l
   					   (lam (flatten (map ess-addr-args l)))))))
   				   (lam (list (ess-utterance-addr u)))))))))
    (generate-utterance-tree tree)
    (send Thecanvas on-paint))


   (ess-addr-deep-args (addr pred)
    (cons addr (if (pred addr) '() (map (lambda (a) (ess-addr-deep-args a pred)) (ess-addr-args addr)))))

   (paint-info (text swap)
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
    (ftglRenderFont Font (substring text 0 (min (string-length text) 120)) 65535)
    (if swap
     (send Thecanvas swap-gl-buffers)
     '())
     (gl-disable 'scissor-test))

   (select (u tree)
    (set! Selected-tree tree)
    (set-whole-tree-selection! tree u)
    (let ((x (+ (whole-tree-offset-x tree) (ess-utterance-x u)))
          (y (+ (whole-tree-offset-y tree) (ess-utterance-y u)))
          (w (ess-utterance-w u))
          (h (ess-utterance-h u)))
     (if
      (or
       (and (negative? (+ x w)) (not VERTICAL))
       (> x (/ (whole-tree-w tree) (whole-tree-zoom tree))))
      (let ((c (+ (ess-utterance-x u) (/ w 2))))
       (set-whole-tree-offset-x! tree (- (+ c (- (/ (whole-tree-w tree) (whole-tree-zoom tree) 2))))))
      '())
     (if
      (or
       (and (negative? (+ y h)) VERTICAL)
       (> y (/ (whole-tree-h tree) (whole-tree-zoom tree))))
      (let ((c (+ (ess-utterance-y u) (/ h 2))))
       (set-whole-tree-offset-y! tree (- (+ c (- (/ (whole-tree-h tree) (whole-tree-zoom tree) 2))))))
      '())))

   (whole-tree-dim (tree)
    (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

   (in? (dim x y)
    (and (> x (car dim)) (> y (cadr dim)) (< x (+ (car dim) (caddr dim))) (< y (+ (cadr dim) (cadddr dim)))))

   (ess-utterance-parent (u tree)
    (apply find-utterance (whole-tree-utterance-tree tree)
     (if VERTICAL
      (list (+ (ess-utterance-x u) -1) (ess-utterance-y u) tree)
      (list (ess-utterance-x u) (+ (ess-utterance-y u) -1) tree))))
   
   (find-utterance (root x y tree)
    (if (or
         (< (min-dim x y) (+ (ess-utterance-min-dim root) (ess-utterance-min-dim-span root)))
         (closed? (ess-utterance-addr root) tree)
         (null? (ess-addr-args (ess-utterance-addr root)))
         (> (maj-dim x y) (let ((baby (last (ess-utterance-args root)))) (+ (ess-utterance-maj-dim baby) (ess-utterance-maj-dim-span baby)))))
     root
     (ormap
      (lambda (child)
       (if (< (maj-dim x y) (+ (ess-utterance-maj-dim child) (ess-utterance-maj-dim-span child)))
        (find-utterance child x y tree)
        #f))
      (ess-utterance-args root))))
  ))

(define Info-dim (list 0 (- HEIGHT 30) WIDTH 30))

(define Font (ftglCreateBitmapFont "/home/philip/olddesktop/vilisp/VeraMono.ttf"))
(ftglSetFontFaceSize Font 12 72)

(define (box-width box)
 (ftglGetFontAdvance Font box))

(define (box-height box)
 (ftglGetFontLineHeight Font))

(define (box-maj-dim box)
 (if VERTICAL (box-height box) (box-width box)))

(define (ess-addr-width addr tree)
 (if VERTICAL (box-width (ess-man-text (ess-addr-man addr))) (ess-addr-maj-dim addr tree)))

(define (ess-addr-height addr tree)
 (if VERTICAL (ess-addr-maj-dim addr tree) CELLHEIGHT))

(define (ess-addr-maj-dim addr tree)
 (if (closed? addr tree)
  (box-maj-dim (ess-man-text (ess-addr-man addr)))
  (max
   (box-maj-dim (ess-man-text (ess-addr-man addr)))
   (foldl
    +
    0
    (map (lambda (arg) (ess-addr-maj-dim arg tree)) (ess-addr-args addr))))))

(define VERTICAL #f)
(define Mouse-pos (cons -1 -1))
(define Info "test")
(define SCROLLDIST 100)
(define Thecanvas (new my-canvas% (parent win)))

(define (ess-utterance-maj-dim u) (if VERTICAL (ess-utterance-y u) (ess-utterance-x u)))
(define (ess-utterance-maj-dim-span u) (if VERTICAL (ess-utterance-h u) (ess-utterance-w u)))
(define (ess-utterance-min-dim u) (if VERTICAL (ess-utterance-x u) (ess-utterance-y u)))
(define (ess-utterance-min-dim-span u) (if VERTICAL (ess-utterance-w u) (ess-utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))


(define (ess-man-text lst)
 (format "~s" (cdr lst)))

(define (rel->gl l)
 (list (car l) (- HEIGHT (+ (cadddr l) (cadr l))) (caddr l) (cadddr l)))



(define (generate-utterance-tree tree)
    (set-whole-tree-utterance-tree! tree (ess-addr->ess-utterance (whole-tree-addr-tree tree) (ess-utterance-x (whole-tree-utterance-tree tree)) (ess-utterance-y (whole-tree-utterance-tree tree)) (if VERTICAL (ess-addr-width (whole-tree-addr-tree tree) tree) -1) 0 1 tree))
    (set-whole-tree-selection! tree (find-utterance-from-laddr (whole-tree-utterance-tree tree) (ess-addr-laddr (ess-utterance-addr (whole-tree-selection tree))))))

(define (find-utterance-from-laddr tree laddr)
 (if (null? laddr)
  tree
  (find-utterance-from-laddr (list-ref (ess-utterance-args tree) (car laddr)) (cdr laddr))))

(define (add-to-screen root childfunc)
 (display-on-screen 0 0 (/ WIDTH 2) 0 root childfunc)
 (normalize-trees))

(define (normalize-trees)
 (let* ((num (length (cddr Trees)))
        (h (round (/ (- HEIGHT 30) num))))
  (for-each
   (lambda (tree n)
    (set-whole-tree-x! tree (/ WIDTH 2))
    (set-whole-tree-y! tree (+ 30 (* (- (+ -1 num) n) h)))
    (set-whole-tree-w! tree (/ WIDTH 2))
    (set-whole-tree-h! tree h))
   (cddr Trees)
   (build-list num identity))))

(define (display-on-screen x y w h root childfunc)
 (let ((tree
  (let* ((addr (root->ess-addr root childfunc '()))
         (dummy-utterance (ess-utterance addr 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
   (whole-tree
    addr
    childfunc
    dummy-utterance
    (set)
    dummy-utterance
    x
    y
    w
    h
    0
    0
    1))))
 (set-whole-tree-utterance-tree! tree (ess-addr->ess-utterance (whole-tree-addr-tree tree) 0 0 w 0 '() tree))
 (set-whole-tree-selection! tree (whole-tree-utterance-tree tree))
 (set! Trees (append Trees (list tree)))))

(define (ess-addr->ess-utterance addr x y w row siblings tree)
 (with
  ((let ((children 
          (if (closed? addr tree)
           '()
           (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (ess-addr-width arg tree)) (ess-addr-args addr))) -1)))
            (caddr
             (foldl
              (lambda (arg data)
               (let ((res (ess-addr->ess-utterance
                           arg
                           (if VERTICAL (+ (car data) w) (car data))
                           (if VERTICAL (cadr data) (+ (cadr data) (ess-addr-height arg tree)))
                           child-w
                           (+ 1 row)
                           (- (length (ess-addr-args addr)) 1)
                           tree)))
                (list
                 (if VERTICAL (car data) (+ (car data) (ess-utterance-w res)))
                 (if VERTICAL (+ (cadr data) (ess-addr-height arg tree)) (cadr data))
                 (if (null? res)
                  (caddr data)
                  (append
                   (caddr data)
                   (list res))))))
              (list x y '())
              (ess-addr-args addr)))))))
    (ess-utterance
     addr
     x
     y
     (if VERTICAL w (max (box-width (ess-man-text (ess-addr-man addr))) (apply + (map ess-utterance-w children))))
     (ess-addr-height addr tree)
     (box-width (ess-man-text (ess-addr-man addr)))
     (box-height (ess-man-text (ess-addr-man addr)))
     children
     (get-color addr siblings))))

  (get-color (addr siblings)
   (if (eq? addr (ess-utterance-addr (whole-tree-selection Selected-tree)))
    SELCOLOR
    (if (eq? COLORSCHEME 'gradient)
     (let* ((pos (if (null? (ess-addr-laddr addr)) 0 (last (ess-addr-laddr addr))))
  	  (diff (/ pos (if (zero? siblings) 1 siblings)))
  	  (col (map + INITIALCOLOR (map round (map * (make-list 3 diff) COLORRANGES)))))
      (if #f ;(null? (ess-man-args (ess-addr-man addr)))
       CODECOLOR1
       (cons (apply make-object color% FGCOLOR) (apply make-object color% col))))
     (let* ((row (length (ess-addr-laddr addr)))
  	  (col (if (null? (ess-addr-laddr addr)) 0 (last (ess-addr-laddr addr)))))
      (if #f ;(null? (ess-man-args (ess-addr-man addr)))
       (if (zero? col) CODECOLOR3 (if (odd? col) CODECOLOR1 CODECOLOR2))
       (if (odd? row)
        (if (zero? col) COLOR5 (if (odd? col) COLOR1 COLOR2))
        (if (zero? col) COLOR6 (if (odd? col) COLOR3 COLOR4))))))))
  ))

(define (root->ess-addr man childlist laddr)
 (ess-addr man laddr
  (delay
   (let ((children (childlist man)))
    (map
     (lambda (arg n) (root->ess-addr arg childlist (append laddr (list n))))
     children
     (build-list (length children) values))))))

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
