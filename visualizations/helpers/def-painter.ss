#lang racket

(require sgl sgl/gl)
(require "../../common.ss")

(provide def-painter)

(define (inv? x y w h tree)
 (or
  (and (< (+ x w) (- (whole-tree-offset-x tree))) (not VERTICAL))
  (and (< (+ y h) (- (whole-tree-offset-y tree))) VERTICAL)
  (> x (- (/ (whole-tree-w tree) (whole-tree-zoom tree)) (whole-tree-offset-x tree)))
  (> y (- (/ (whole-tree-h tree) (whole-tree-zoom tree)) (whole-tree-offset-y tree)))))

;(define (center offset lenwhole lenpiece start width)
; (let ((visible-width (- (min (+ offset lenwhole) (+ start width)) (max offset start))))
;  (if (< visible-width lenpiece)
;   (if (< offset start)
;    (- (+ offset lenwhole) lenpiece)
;    offset)
;   (+ (max offset start) (/ visible-width 2) (- (/ lenpiece 2))))))

(define image-dir "/home/philip/olddesktop/vilisp/vilisp/semantic/images/")

(define (def-painter #:drawer (drawer values) #:invisible? (invisible? inv?))
 (define image '())
 (define images '())
 (define repetitions 1)
; (define image-files '("galaxies.png"))
; (define image-files '("360milky_way_over_tenerife.jpg"))
 (define image-files (map (lambda (n) (string-append "time/" (number->string n) ".png")) (build-list 100 (curry + 1))))
 (define images-n (length image-files))
 (define images-dimensions (make-list images-n (map string->number (string-split (with-output-to-string (lambda () (system* "/usr/bin/identify" "-format" "%w %h" (string-append image-dir (car image-files)))))))))
; (define images-dimensions (map
;                            (lambda (img-n)
;                             (map string->number (string-split (with-output-to-string (lambda () (system* "/usr/bin/identify" "-format" "%w %h" (string-append image-dir "time/" (number->string img-n) ".png")))))))
;                            (build-list n (curry + 1))))
 (lambda (tree)
  (with
   (
    (if (null? images)
     (begin
      (glPixelStorei GL_UNPACK_ALIGNMENT 1)
      (map (lambda (img-f)
            (set! images (cons (SOIL_load_OGL_texture (string-append image-dir img-f) 0 0 4) images)))
           image-files)
      (set! images (reverse images)))
     '())
    (apply gl-scissor (whole-tree-dim tree))
    (apply gl-viewport (whole-tree-dim tree))

    (define distance 10)

    (gl-matrix-mode 'projection)
    (gl-load-identity)
    (gl-frustum
     0
     (whole-tree-w tree)
     (- (whole-tree-h tree))
     0
     1
     (* (/ (whole-tree-zoom tree)) distance))
    (gl-translate (whole-tree-offset-x tree) (- (whole-tree-offset-y tree)) (- 1 (/ (whole-tree-zoom tree))))
    (gl-matrix-mode 'modelview)
    (gl-load-identity)

    (glEnable GL_TEXTURE_2D)
    (gl-clear 'color-buffer-bit)

    (foldl
     (lambda (img img-d d)
      (let* ((x (car d))
             (next-x (+ x (car img-d)))
             (y (cdr d))
             (next-y (- (+ y (cadr img-d)))))
       (glBindTexture GL_TEXTURE_2D img)
       (gl-begin 'quads)
       (gl-tex-coord 0 0)
       (gl-vertex (* x distance) (* distance (- y)) (- distance))
       (gl-tex-coord repetitions 0)
       (gl-vertex (* distance next-x) (* distance (- y)) (- distance))
       (gl-tex-coord repetitions 1)
       (gl-vertex (* distance next-x) (* distance next-y) (- distance))
       (gl-tex-coord 0 1)
       (gl-vertex (* distance x) (* distance next-y) (- distance))
       (gl-end)
       (if (> x 50000) (cons 0 next-y) (cons next-x y))))
     '(0 . 0)
     images
     images-dimensions)

    (glDisable GL_TEXTURE_2D)

    (utterance-paint (whole-tree-utterance-tree tree) tree)
    )

   (utterance-paint (u tree)
    (let* ((text ((node-text-func (utterance-node u)) (utterance-node u)))
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
       (drawer text x y w h text-w text-h clr u tree center)
;       (draw-rectangle (if (eq? Selected-tree tree) (cdr clr) (map (curryr / 3) (cdr clr))) x y w h)
;       (if (< (whole-tree-zoom tree) 1) '()
;        (draw-text
;         text
;         (* (whole-tree-zoom tree) (center x w (- text-w PADDING) (- (whole-tree-offset-x tree)) (whole-tree-w tree)))
;         (* (whole-tree-zoom tree) (+ text-h -3 (center y h text-h (- (whole-tree-offset-y tree)) (whole-tree-h tree))))
;         (car clr)
;         tree))
       (for-each touch (map (lambda (arg) (future (lambda () (utterance-paint arg tree)))) args)))))))))


