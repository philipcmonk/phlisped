#lang racket

(require sgl sgl/gl)

(require ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector)

(define-ffi-definer define-ftgl (ffi-lib "libftgl"))

(define _FTGLfont (_cpointer 'FTGLfont))
(define _GLuint _uint)
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

(define-ffi-definer define-soil (ffi-lib "libsoil"))

(define-soil SOIL_load_OGL_texture (_fun _path _int _uint _uint -> _uint))
(define-soil SOIL_last_result (_fun -> _string))

(provide (all-defined-out))

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(struct node (data laddr prom-args text-func) #:transparent)
(struct utterance (node x y w h text-w text-h args clr) #:transparent)
(struct whole-tree (n-tree childfunc utterance-tree open selection x y w h v11n offset-x offset-y zoom) #:mutable)
(struct v11n (paint-tree node->v11n-utterance find-utterance wheel))

(define Selected-tree '())
(define (set-selected-tree tree)
 (set! Selected-tree tree))

(define Font #f)
(define (set-font f)
 (set! Font f))

(define PADDING 5)
(define VERTICAL #f)
(define CELLHEIGHT 25)
(define SCROLLDIST 100)

(define node-args (compose force node-prom-args))

(define (node-width n tree)
 (if VERTICAL (box-width ((node-text-func n) (node-data n))) (node-maj-dim n tree)))

(define (node-height n tree)
 (if VERTICAL (node-maj-dim n tree) CELLHEIGHT))

(define (node-maj-dim n tree)
 (if (closed? n tree)
  (box-maj-dim ((node-text-func n) (node-data n)))
  (max
   (box-maj-dim ((node-text-func n) (node-data n)))
   (foldl
    +
    0
    (map (lambda (arg) (node-maj-dim arg tree)) (node-args n))))))

(define (whole-tree-selection-u tree)
 (find-utterance-from-laddr-safe (whole-tree-utterance-tree tree) (whole-tree-selection tree)))

(define (find-utterance-from-laddr-safe tree laddr)
 (if (null? laddr)
  tree
  (if (> (length (utterance-args tree)) (car laddr))
   (find-utterance-from-laddr-safe (list-ref (utterance-args tree) (car laddr)) (cdr laddr))
   #f)))

(define (draw-rectangle clr x y w h)
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))

 (gl-begin 'quads)
 (gl-vertex x (- y) -1.01)
 (gl-vertex (+ x w) (- y) -1.01)
 (gl-vertex (+ x w) (- (+ y h)) -1.01)
 (gl-vertex x (- (+ y h)) -1.01)
 (gl-end))

(define (draw-text text x y clr (tree '()))
 (gl-color (/ (car clr) 255) (/ (cadr clr) 255) (/ (caddr clr) 255))
 (gl-push-matrix)
 (gl-translate x (- y) -1.01)
 (ftglRenderFont Font text 65535)
 (gl-pop-matrix))

(define (whole-tree-dim tree)
 (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

(define (box-width box)
 (+ PADDING (ftglGetFontAdvance Font box)))

(define (box-height box)
 (ftglGetFontLineHeight Font))

(define (box-maj-dim box)
 (if VERTICAL (box-height box) (box-width box)))

(define (open? n tree) (set-member? (whole-tree-open tree) (node-laddr n)))
(define closed? (negate open?))

(define get-color (lambda (a94 a95) (letrec ((v432 0.6180339887498949) (v354 (lambda (a359 a364) (letrec ((v358 a359)) (- v358 (letrec ((v363 a364)) (* v363 (truncate (/ v358 v363)))))))) (v206 (lambda (a222 a229 a230) (letrec ((v221 a222) (v223 (letrec ((v227 a230) (v228 a229)) (* v227 v228))) (v231 (* v223 (- 1 (abs (- (v354 (* v221 6) 2) 1)))))) (cond ((< v221 (/ 1 6)) (list v223 v231 0)) ((< v221 (/ 2 6)) (list v231 v223 0)) ((< v221 (/ 3 6)) (list 0 v223 v231)) ((< v221 (/ 4 6)) (list 0 v231 v223)) ((< v221 (/ 5 6)) (list v231 0 v223)) ((< v221 (/ 6 6)) (list v223 0 v231)))))) (v35 a95) (v26 a94) (v96 (cons (quote (0 0 0)) (map (curry * 255) (v206 0.15 1.0 1.0))))) (if (equal? (node-laddr v26) (whole-tree-selection v35)) v96 (letrec ((v110 (cons (quote (0 0 0)) (map (curry * 255) (v206 0.15 0.9 0.9))))) (if (equal? (car (node-data v26)) (car (node-data (utterance-node (whole-tree-selection-u v35))))) v110 (letrec ((v450 (lambda () (v354 (* (letrec ((v452 (lambda (a462) (letrec ((v461 a462)) (if (null? v461) 0 (last v461)))))) (v452 (node-laddr v26))) v432) 1))) (v85 (if (null? (node-laddr v26)) 0 (last (node-laddr v26)))) (v188 (cons (quote (255 255 255)) (quote (80 0 0)))) (v373 (cons (quote (255 255 255)) (map (curry * 255) (v206 (v450) 0.8 0.8)))) (v405 (cons (quote (255 255 255)) (map (curry * 255) (v206 (v450) 0.6 0.8))))) (if (odd? (length (node-laddr v26))) v373 v405))))))))

(define (utterance-maj-dim u) (if VERTICAL (utterance-y u) (utterance-x u)))
(define (utterance-maj-dim-span u) (if VERTICAL (utterance-h u) (utterance-w u)))
(define (utterance-min-dim u) (if VERTICAL (utterance-x u) (utterance-y u)))
(define (utterance-min-dim-span u) (if VERTICAL (utterance-w u) (utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))

(define (center offset lenwhole lenpiece start width)
 (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
  (if (< visible-width lenpiece)
   (if (< offset start)
    (- (+ offset lenwhole) lenpiece)
    offset)
   (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))

(define (replace t1 t2s es)
 (append (takef es (negate (curry equal? t1))) t2s (cdr (member t1 es))))

