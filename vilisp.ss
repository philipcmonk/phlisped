#lang racket

(require racket/draw)
(require racket/generator)
(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector)
(require (except-in racket/gui yield ->))
(require (only-in racket/gui (yield yield-gui) (-> ->-gui)))
(require sgl sgl/bitmap sgl/gl)

(provide Thecanvas)


(define-ffi-definer define-ftgl (ffi-lib "libftgl"))

(define _FTGLfont (_cpointer 'FTGLfont))
(define-ftgl ftglCreatePixmapFont (_fun _path -> _FTGLfont))
(define-ftgl ftglSetFontFaceSize (_fun _FTGLfont _int _int -> _void))
(define-ftgl ftglGetFontLineHeight (_fun _FTGLfont -> _float))
(define-ftgl ftglGetFontAdvance (_fun _FTGLfont _string -> _float))
(define-ftgl ftglRenderFont (_fun _FTGLfont _string _int -> _void))
(define-ftgl ftglDestroyFont (_fun _FTGLfont -> _void))


;------------------------------------------------------------------------------
; User-defined constants
;------------------------------------------------------------------------------

(define VERTICAL #f)
(define STARTOPEN #f)
(define WIDTH (* 1 1600))
(define HEIGHT 899)
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
(define BGCOLOR "black")
(define INITIALCOLOR '(0 0 127))
(define COLORRANGES '(127 0 -127))
(define FGCOLOR '(255 255 255))
(define CELLWIDTH 150)
(define CELLHEIGHT 35)
(define SCROLLDIST 100)
(define FILENAME "vilisp.ss")
;(define FILENAME "/home/philip/semanticweb/lisp/graph.ss")

;(define COLOR1 "darkblue")
;(define COLOR2 "yellow")
;(define COLOR3 "red")
;(define COLOR4 "cyan")
;(define SELCOLORb "darkgreen")
;(define SELCOLORf "magenta")

(define (concat s n)
 (string->symbol (string-append s (number->string n))))
(define ROLES
 (apply
  hash
  `(if ((() cond on-true on-false) ())
	  lambda ((() args) ())
	  define ((() identifier) ())
	  map ((() proc) ,(curry concat "list"))
	  for-each ((() proc) ,(curry concat "list"))
	  foldl ((() proc init) ,(curry concat "list"))
	  foldr ((() proc init) ,(curry concat "list"))
	  filter ((() proc) ,(curry concat "list"))
	  andmap ((() proc) ,(curry concat "list"))
	  ormap ((() proc) ,(curry concat "list")))))

(define Roles ROLES)

;------------------------------------------------------------------------------
; Utility functions
;------------------------------------------------------------------------------

(define (border?) (eq? COLORSCHEME 'gradient))
(define (get-color addr siblings)
 (if (eq? addr (ess-utterance-addr Selection))
  SELCOLOR
  (if (eq? COLORSCHEME 'gradient)
   (let* ((pos (if (null? (ess-addr-laddr addr)) 0 (last (ess-addr-laddr addr))))
	  (diff (/ pos (if (zero? siblings) 1 siblings)))
	  (col (map + INITIALCOLOR (map round (map * (make-list 3 diff) COLORRANGES)))))
    (if (null? (ess-man-args (ess-addr-man addr)))
     CODECOLOR1
     (cons (apply make-object color% FGCOLOR) (apply make-object color% col))))
   (let* ((row (length (ess-addr-laddr addr)))
	  (col (if (null? (ess-addr-laddr addr)) 0 (last (ess-addr-laddr addr)))))
    (if (null? (ess-man-args (ess-addr-man addr)))
     (if (zero? col) CODECOLOR3 (if (odd? col) CODECOLOR1 CODECOLOR2))
     (if (odd? row)
      (if (zero? col) COLOR5 (if (odd? col) COLOR1 COLOR2))
      (if (zero? col) COLOR6 (if (odd? col) COLOR3 COLOR4))))))))
(define (center offset lenwhole lenpiece start width)
 (let ((naive (+ (max offset start) (/ (min lenwhole (+ lenwhole offset) width (- (+ start width) offset)) 2) (- (/ lenpiece 2))))
       (start-bound (- (+ offset lenwhole) lenpiece)))
  (cond
   ((> naive start-bound) start-bound)
   ((> (+ naive lenpiece) (+ start width)) offset)
   (#t naive))))
(define (ess-expr-descendants expr)
 (cons expr (map ess-expr-descendants (ess-expr-args expr))))
(define (ess-man-descendants man)
 (cons man (map ess-man-descendants (ess-man-args man))))
(define (ess-addr-deep-args addr pred)
 (cons addr (if (pred addr) '() (map (lambda (a) (ess-addr-deep-args a pred)) (ess-addr-args addr)))))
(define (inc lst pos)
 (if (> pos 0)
  (append
   (take lst pos)
   (list (+ 1 (list-ref lst pos)))
   (drop lst (+ 1 pos)))
  (cons (+ 1 (car lst)) (cdr lst))))
(define (ess-utterance-parent u)
 (apply find-utterance Utterance-tree
  (if VERTICAL
   (list (+ (ess-utterance-x u) -1) (ess-utterance-y u))
   (list (ess-utterance-x u) (+ (ess-utterance-y u) -1)))))
(define (ess-utterance-maj-dim u) (if VERTICAL (ess-utterance-y u) (ess-utterance-x u)))
(define (ess-utterance-maj-dim-span u) (if VERTICAL (ess-utterance-h u) (ess-utterance-w u)))
(define (ess-utterance-min-dim u) (if VERTICAL (ess-utterance-x u) (ess-utterance-y u)))
(define (ess-utterance-min-dim-span u) (if VERTICAL (ess-utterance-w u) (ess-utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))
(define (get-role type n)
 (if (hash-has-key? Roles type)
  (let ((r (hash-ref Roles type)))
   (if (< n (length (car r)))
    (list-ref (car r) n)
    (if (null? (cadr r))
     '()
     ((cadr r) (- n (length (car r)))))))
  '()))
(define (invisible x y w h)
 (or
  (and (< (+ x w) (- Scroll-offset-x)) (not VERTICAL))
  (and (< (+ y h) (- Scroll-offset-y)) VERTICAL)
  (> x (- (/ WIDTH Zoom-factor) Scroll-offset-x))
  (> y (- (/ WIDTH Zoom-factor) Scroll-offset-y))))
(define (ess-addr-args addr)
 (force (ess-addr-prom-args addr)))
(define (ess-utterance-args u)
 (force (ess-utterance-prom-args u)))
(define (get-ess-addr addr laddr)
 (if (null? laddr)
  addr
  (get-ess-addr (list-ref (ess-addr-args addr) (car laddr)) (cdr laddr))))
(define (open? addr)
 (set-member? Open addr))
(define (closed? addr)
 (not (open? addr)))

;------------------------------------------------------------------------------
; Struct/class declarations
;------------------------------------------------------------------------------

(struct ess-expr (name args type)) 
(struct ess-man (expr text args role context))
(struct ess-addr (man laddr prom-args))
(struct ess-utterance (addr x y w h prom-args clr)) 

(define (resize w h)
 (gl-viewport 0 0 w h)
 #t)

(define my-canvas%
 (class* canvas% ()
  (inherit with-gl-context swap-gl-buffers)

  (define/override (on-paint)
   (with-gl-context
    (lambda ()
     (gl-clear-color 0.0 0.0 0.0 0.0)
     (gl-clear 'color-buffer-bit)

     (gl-matrix-mode 'projection)
     (gl-load-identity)
     (gl-ortho (- Scroll-offset-x) (+ (- Scroll-offset-x) (/ WIDTH Zoom-factor)) (+ Scroll-offset-y (- (/ HEIGHT Zoom-factor))) Scroll-offset-y -1.0 1.0)
     (gl-matrix-mode 'modelview)
     (gl-load-identity)

     (ess-utterance-paint Utterance-tree (send Thecanvas get-dc))
     (swap-gl-buffers))))

  (define/override (on-size width height)
   (with-gl-context
    (lambda ()
     (resize width height))))

  (define/override (on-event event)
   (let* ((x (- (/ (send event get-x) Zoom-factor) Scroll-offset-x))
	  (y (- (/ (send event get-y) Zoom-factor) Scroll-offset-y))
	  (clicked (find-utterance Utterance-tree x y)))
    (cond
     ((send event dragging?)
      (cond
       ((send event get-left-down)
	(set! Scroll-offset-x (+ Scroll-offset-x (/ (+ (- (car Mouse-pos)) (send event get-x)) Zoom-factor)))
	(set! Scroll-offset-y (+ Scroll-offset-y (/ (+ (- (cdr Mouse-pos)) (send event get-y)) Zoom-factor)))
	(set! Mouse-pos (cons (send event get-x) (send event get-y)))
	(send this on-paint))))
     ((eq? (send event get-event-type) 'left-down)
      (set! Mouse-pos (cons (send event get-x) (send event get-y))))
     ((eq? (send event get-event-type) 'left-up)
      (select clicked)
      (generate-utterance-tree ARGS)
      (send this on-paint))
     ((eq? (send event get-event-type) 'middle-down)
      (select clicked)
      (close-u Selection (send event get-control-down)))
     ((eq? (send event get-event-type) 'right-down)
      (select clicked)
      (open-u Selection (send event get-control-down)))
     (#t '()))))

  (define/override (on-char event)
   (cond
    ((eq? (send event get-key-code) #\f)
     (begin (set! VERTICAL (not VERTICAL)) (generate-utterance-tree ARGS) (send this on-paint)))
    ((eq? (send event get-key-code) #\F)
     (begin (set! COLORSCHEME (if (eq? COLORSCHEME 'gradient) 'alternate 'gradient)) (generate-utterance-tree ARGS) (send this on-paint)))
    ((eq? (send event get-key-code) #\h)
     (go 'left))
    ((eq? (send event get-key-code) #\j)
     (go 'down))
    ((eq? (send event get-key-code) #\k)
     (go 'up))
    ((eq? (send event get-key-code) #\l)
     (go 'right))
    ((eq? (send event get-key-code) #\o)
     (open-u Selection #f))
    ((eq? (send event get-key-code) #\c)
     (close-u Selection #f))
    ((eq? (send event get-key-code) #\O)
     (open-u Selection #t))
    ((eq? (send event get-key-code) #\C)
     (close-u Selection #t))
    ((eq? (send event get-key-code) #\z)
     (set! Scroll-offset-y (- (ess-utterance-y Selection)))
     (set! Scroll-offset-x (- (ess-utterance-x Selection)))
     (set! Zoom-factor (if (= Zoom-factor 1) (if VERTICAL (/ HEIGHT (ess-utterance-h Selection)) (/ WIDTH (ess-utterance-w Selection ))) 1))
     (send this on-paint))
    ((eq? (send event get-key-code) 'wheel-up)
     (if VERTICAL
      (set! Scroll-offset-y (+ SCROLLDIST Scroll-offset-y))
      (set! Scroll-offset-x (+ SCROLLDIST Scroll-offset-x)))
     (send this on-paint))
    ((eq? (send event get-key-code) 'wheel-down)
     (if VERTICAL
      (set! Scroll-offset-y (+ (- SCROLLDIST) Scroll-offset-y))
      (set! Scroll-offset-x (+ (- SCROLLDIST) Scroll-offset-x)))
     (send this on-paint))
    ((eq? (send event get-key-code) 'wheel-left)
     (if VERTICAL
      (set! Scroll-offset-x (+ SCROLLDIST Scroll-offset-x))
      (set! Scroll-offset-y (+ SCROLLDIST Scroll-offset-y)))
     (send this on-paint))
    ((eq? (send event get-key-code) 'wheel-right)
     (if VERTICAL
      (set! Scroll-offset-x (+ (- SCROLLDIST) Scroll-offset-x))
      (set! Scroll-offset-y (+ (- SCROLLDIST) Scroll-offset-y)))
     (send this on-paint))))

  (super-instantiate () (style '(gl)))))

(define win (new frame% (label "vilisp") (min-width WIDTH) (min-height HEIGHT)))
(define Thecanvas (new my-canvas% (parent win)))

(define (generate-utterance-tree addr)
    (set! Rows '())
    (set! Utterance-tree (ess-addr->ess-utterance addr 0 0 (if VERTICAL (ess-addr-width ARGS (send Thecanvas get-dc)) -1) 0 (send Thecanvas get-dc) 1))
    (set! Selection (find-utterance-from-addr Utterance-tree (ess-utterance-addr Selection))))

;------------------------------------------------------------------------------
; Conversion functions
;------------------------------------------------------------------------------

(define (list->ess-expr l)
 (apply
  ess-expr
  (cond
   ((symbol? l) (list (symbol->string l) '() 'symbol))
   ((string? l) (list l '() 'string))
   ((number? l) (list (number->string l) '() 'number))
   ((null? l) (list "null" '() 'null))
   ((list? l)
    (list
     (ess-expr-name (list->ess-expr (car l)))
     (map list->ess-expr l)
     (if (symbol? (car l)) (car l) 'list)))
   (#t (list "--" '() 'unknown)))))

(define (ess-expr->ess-man expr role laddr context)
 (ess-man
  expr
  (string-append
   (if (null? role) "" (string-append (symbol->string role) ":  "))
   (ess-expr-name expr)
   (if (eq? (ess-expr-type expr) 'define) (string-append ":  " (ess-expr-name (cadr (ess-expr-args expr)))) ""))
  (let ((new-context
	 (apply hash-set* context
	  (apply append
	   (map
	    (lambda (arg)
	     (let ((id (cadr (ess-expr-args (car arg)))))
	      (list (string->symbol (ess-expr-name id)) (append laddr (cdr arg)))))
	    (filter
	     (lambda (arg) (eq? (ess-expr-type (car arg)) 'define))
	     (map list (ess-expr-args expr) (build-list (length (ess-expr-args expr)) values))))))))
   (map
    (lambda (arg n)
     (ess-expr->ess-man
      arg
      (get-role (ess-expr-type expr) n)
      (append laddr (list n))
      new-context))
    (ess-expr-args expr)
    (build-list (length (ess-expr-args expr)) values)))
  role
  context))

(define (ess-man->ess-addr man laddr)
 (ess-addr man laddr
  (delay
   (if (and
	(eq? (ess-expr-type (ess-man-expr man)) 'symbol)
	(not (eq? (ess-man-role man) 'identifier))
	(not (eq? (ess-man-role (ess-addr-man (get-ess-addr ARGS (drop-right laddr 1)))) 'identifier))
	(hash-has-key? (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
    (let* ((ref-laddr (hash-ref (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
	   (ref-addr (get-ess-addr ARGS ref-laddr)))
     (list (ess-man->ess-addr
	    (ess-addr-man ref-addr)
	    (append laddr (list 0)))))
    (map
     (lambda (arg n) (ess-man->ess-addr arg (append laddr (list n))))
     (ess-man-args man)
     (build-list (length (ess-man-args man)) values))))))

(define (ess-addr->ess-utterance addr x y w row dc siblings)
 (let ((children 
	(if (closed? addr)
	 '()
	 (let ((child-w (if VERTICAL (foldl max 0 (map (lambda (arg) (ess-addr-width arg dc)) (ess-addr-args addr))) -1)))
	  (caddr
	   (foldl
	    (lambda (arg data)
	     (let ((res (ess-addr->ess-utterance
			 arg
			 (if VERTICAL (+ (car data) w) (car data))
			 (if VERTICAL (cadr data) (+ (cadr data) (ess-addr-height arg dc)))
			 child-w
			 (+ 1 row)
			 dc
			 (- (length (ess-addr-args addr)) 1))))
	      (list
	       (if VERTICAL (car data) (+ (car data) (ess-utterance-w res)))
	       (if VERTICAL (+ (cadr data) (ess-addr-height arg dc)) (cadr data))
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
   (ess-addr-height addr dc)
   children
   (get-color addr siblings))))

;------------------------------------------------------------------------------
; Find functions
;------------------------------------------------------------------------------

(define (find-utterance root x y)
 (if (or
      (< (min-dim x y) (+ (ess-utterance-min-dim root) (ess-utterance-min-dim-span root)))
      (closed? (ess-utterance-addr root))
      (null? (ess-addr-args (ess-utterance-addr root)))
      (> (maj-dim x y) (let ((baby (last (ess-utterance-args root)))) (+ (ess-utterance-maj-dim baby) (ess-utterance-maj-dim-span baby)))))
  root
  (ormap
   (lambda (child)
    (if (< (maj-dim x y) (+ (ess-utterance-maj-dim child) (ess-utterance-maj-dim-span child)))
     (find-utterance child x y)
     #f))
   (ess-utterance-args root))))

(define (find-utterance-from-addr tree addr)
 (if (eq? (ess-utterance-addr tree) addr)
  tree
  (ormap
   (lambda (child) (find-utterance-from-addr child addr))
   (ess-utterance-args tree))))

;------------------------------------------------------------------------------
; Paint functions
;------------------------------------------------------------------------------

(define (ess-utterance-paint u dc)
 (let* ((text (ess-man-text (ess-addr-man (ess-utterance-addr u))))
	(x (ess-utterance-x u))
	(y (ess-utterance-y u))
	(w (ess-utterance-w u))
	(h (ess-utterance-h u))
	(args (ess-utterance-args u))
	(clr (ess-utterance-clr u)))
  (if (invisible x y w h)
   '()
   (begin
    (draw-rectangle (cdr clr) x y w h)
    (if (< Zoom-factor 1) '()
     (draw-text
      text
      (* Zoom-factor (center x w (box-width text) (- Scroll-offset-x) WIDTH))
      (* Zoom-factor (let ((box-h (box-height text))) (+ box-h -3 (center y h box-h 0 HEIGHT))))
      (car clr)))
    (for-each (lambda (arg) (ess-utterance-paint arg dc)) args)))))

(define (draw-rectangle clr x y w h)
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))

 (gl-begin 'quads)
 (gl-vertex x (- y))
 (gl-vertex (+ x w) (- y))
 (gl-vertex (+ x w) (- (+ y h)))
 (gl-vertex x (- (+ y h)))
 (gl-end))

(define (draw-text text x y clr)
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))
 (gl-raster-pos (- 1 Scroll-offset-x) (- Scroll-offset-y 1))
 (glBitmap 0 0 0 0 (+ x Scroll-offset-x) (- (+ y Scroll-offset-y)) (make-cvector _byte 1))
 (ftglRenderFont Font text 65535))

;------------------------------------------------------------------------------
; Dimensions functions
;------------------------------------------------------------------------------

(define (box-width box)
 (ftglGetFontAdvance Font box))
; (let-values (((tw th a b) (send dc get-text-extent box))) tw))

(define (box-height box)
 (ftglGetFontLineHeight Font))
; (let-values (((tw th a b) (send dc get-text-extent box))) th))

(define (box-maj-dim box)
 (if VERTICAL (box-height box) (box-width box)))

(define (ess-addr-width addr dc)
 (if VERTICAL (box-width (ess-man-text (ess-addr-man addr))) (ess-addr-maj-dim addr dc)))

(define (ess-addr-height addr dc)
 (if VERTICAL (ess-addr-maj-dim addr dc) CELLHEIGHT))

(define (ess-addr-maj-dim addr dc)
 (if (closed? addr)
  (box-maj-dim (ess-man-text (ess-addr-man addr)))
  (max
   (box-maj-dim (ess-man-text (ess-addr-man addr)))
   (foldl
    +
    0
    (map (lambda (arg) (ess-addr-maj-dim arg dc)) (ess-addr-args addr))))))

;------------------------------------------------------------------------------
; Selection functions
;------------------------------------------------------------------------------

(define (go dir)
 (let ((new-sel (apply find-utterance Utterance-tree
		 (cond
		  ((eq? dir 'left)
		   (list (+ (ess-utterance-x Selection) -1) (ess-utterance-y Selection)))
		  ((eq? dir 'down)
		   (list (ess-utterance-x Selection) (+ (ess-utterance-y Selection) (ess-utterance-h Selection) 1)))
		  ((eq? dir 'up)
		   (list (ess-utterance-x Selection) (+ (ess-utterance-y Selection) -1)))
		  ((eq? dir 'right)
		   (list (+ (ess-utterance-x Selection) (ess-utterance-w Selection) 1) (ess-utterance-y Selection)))))))
 (select (if new-sel new-sel Selection)))
 (generate-utterance-tree ARGS)
 (send Thecanvas on-paint))

(define (open-u u deep?)
 (set! Open (set-union Open (list->set
			     (if deep?
			      (flatten (ess-addr-deep-args (ess-utterance-addr u) (lambda (addr) (null? (ess-man-args (ess-addr-man addr))))))
			      (letrec
			       ((lam (lambda (l)
				      (if (or (null? l) (ormap (lambda (x) (closed? x)) l))
				       l
				       (lam (flatten (map ess-addr-args l)))))))
			       (lam (list (ess-utterance-addr u))))))))
  (generate-utterance-tree ARGS)
  (send Thecanvas on-paint))

(define (close-u u deep?)
 (set! Open (set-subtract Open (list->set
				(if deep?
				 (let ((remnant (if (closed? (ess-utterance-addr u)) (ess-utterance-parent u) u)))
				  (select remnant)
				  (flatten (ess-addr-deep-args (ess-utterance-addr remnant) closed?)))
				 (if (closed? (ess-utterance-addr u))
				  (begin
				   (select (ess-utterance-parent u))
				   (flatten (ess-addr-deep-args (ess-utterance-addr (ess-utterance-parent u)) closed?)))
				  (letrec
				   ((lam (lambda (l)
					  (if (andmap
					       (lambda (x) (or (closed? x) (null? (ess-addr-args x))))
					       (flatten (map ess-addr-args l)))
					   l
					   (lam (flatten (map ess-addr-args l)))))))
				   (lam (list (ess-utterance-addr u)))))))))
 (generate-utterance-tree ARGS)
 (send Thecanvas on-paint))

(define (select u)
 (set! Selection u)
 (let ((x (+ Scroll-offset-x (ess-utterance-x u)))
       (y (+ Scroll-offset-y (ess-utterance-y u)))
       (w (ess-utterance-w u))
       (h (ess-utterance-h u)))
  (if
   (or
    (and (negative? (+ x w)) (not VERTICAL))
    (> x (/ WIDTH Zoom-factor)))
   (let ((c (+ (ess-utterance-x u) (/ w 2))))
    (set! Scroll-offset-x (- (+ c (- (/ WIDTH Zoom-factor 2))))))
   '())
  (if
   (or
    (and (negative? (+ y h)) VERTICAL)
    (> y (/ HEIGHT Zoom-factor)))
   (let ((c (+ (ess-utterance-y u) (/ h 2))))
    (set! Scroll-offset-y (- (+ c (- (/ HEIGHT Zoom-factor 2))))))
   '())))


;------------------------------------------------------------------------------
; Global variables and programmatically-defined constants
;------------------------------------------------------------------------------

(define ARGS
 (ess-man->ess-addr
  (ess-expr->ess-man
   (list->ess-expr
    (call-with-input-file FILENAME
     (lambda (f)
      (read-language f)
      (define (in rem)
       (let ((x (read rem)))
	(if (eof-object? x)
	 '(end)
	 (cons x (in rem)))))
      (in f))))
   'root
   '()
   (hash))
  (list)))
(define Rows '())
(define Utterance-tree #f)
(define Root ARGS)
(define Open (set))
(define Selection (ess-utterance ARGS 0 0 0 0 '() #f))
(define Scroll-offset-x 0)
(define Scroll-offset-y 0)
(define Code-gen (generator () (let loop ((x 0)) (yield x) (loop (+ 1 x)))))
(define Gens '())
(define Mouse-pos (cons -1 -1))
(define Zoom-factor 1)
(define Font (ftglCreatePixmapFont "/home/philip/vilisp/VeraMono.ttf"))

;------------------------------------------------------------------------------
; Preparados, listos, ya
;------------------------------------------------------------------------------

(ftglSetFontFaceSize Font 24 24)
(generate-utterance-tree ARGS)
(send win show #t)

