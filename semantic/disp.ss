#lang racket

(require (except-in racket/gui yield ->))
(require (only-in racket/gui (yield yield-gui) (-> ->-gui)))
(require sgl sgl/gl)

(require "common.ss")
;(require "default-horizontal-v11n.ss")
;(require "default-vertical-v11n.ss")
;(require "other-v11n.ss")
;(require "other2-v11n.ss")
;(require "treemap-v11n.rkt")

(provide my-canvas% box-width box-height box-maj-dim node-width node-height node-maj-dim VERTICAL display-on-screen add-to-screen Thecanvas Info Selected-tree utterance-parent utterance-node utterance-args node-data node-laddr whole-tree-selection-u whole-tree-selection set-whole-tree-selection! whole-tree-open set-whole-tree-open! whole-tree-utterance-tree add-key-evs key-evs update-childfuncs set-info enter-insert-mode exit-insert-mode enter-scope-mode exit-scope-mode enter-argify-mode exit-argify-mode enter-search-mode exit-search-mode enter-paste-mode exit-paste-mode set-search-results Search-results show-search-tree scroll-search-results remove-search-tree paint-info semantic-go find-utterance-from-laddr-safe for-all-trees)

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

(define win (new frame% (label "vilisp") (min-width WIDTH) (min-height HEIGHT)))

(require (for-syntax racket/system))
(define-syntax (require-dir syn)
 (let* ((dir (cadr (syntax->datum syn)))
        (phls (map (lambda (f) (string-append dir "/" f)) (filter (lambda (f) (regexp-match ".phl$" f)) (map path->string (directory-list dir))))))
  (for-each (lambda (phl) (system* "bin/phlisp" (string-append "-o " (regexp-replace ".phl$" phl ".rkt")) phl)) phls)
  (let ((rkts (map (lambda (f) (string-append dir "/" f)) (filter (lambda (f) (regexp-match ".rkt$" f)) (map path->string (directory-list dir))))))
   (datum->syntax syn `(require ,@rkts)))))

(require-dir "visualizations")
(define v11ns (list default-horizontal-v11n default-vertical-v11n treemap-v11n other-v11n other2-v11n))

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
                       (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 0 0 0 0 default-horizontal-v11n 0 0 1)))
               (apply whole-tree 
                      (let* ((dummy-n (node '(0 'dummy-bar "dummy bar" () () () () ()) '() (delay '()) (lambda (_) "r")))
                             (dummy-utterance (utterance dummy-n 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
                       (list dummy-n (lambda (a) '()) dummy-utterance (set) '() 800 30 (- WIDTH 600) 300 other-v11n 0 0 1)))))
(set-selected-tree (cadr Trees))
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
        (set-selected-tree next)
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
    #\o (lambda (_) (open-u (whole-tree-selection-u Selected-tree) #f Selected-tree) (send Thecanvas on-paint))
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
    (set-selected-tree tree))
   (generate-utterance-tree Selected-tree)
   (send Thecanvas on-paint))

  (select-new-tree (event)
   (set-selected-tree
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
    (with-gl-context
      (lambda ()
       (gl-clear-color 0.0 0.0 0.0 0.0)
       (gl-clear 'color-buffer-bit)

       (paint-info Info #f)
       (paint-bar (whole-tree-selection-u Selected-tree))
 
       (gl-enable 'scissor-test)
 
       (for-each touch (map (lambda (tree) (future (lambda () ((v11n-paint-tree (whole-tree-v11n tree)) tree)))) Trees))
 
       (gl-disable 'scissor-test)
       (swap-gl-buffers))))
  
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
     (PASTEMODE ((hash-ref key-evs 'paste) event))
     ((hash-has-key? key-evs (send event get-key-code))
      ((hash-ref key-evs (send event get-key-code)) event))
     (#t '())))

   (super-instantiate () (style '(gl)))

   (with-gl-context
    (lambda ()
     (initialize-font)))))))

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

(define PASTEMODE #f)

(define (enter-paste-mode) (set! PASTEMODE #t))

(define (exit-paste-mode) (set! PASTEMODE #f))

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


;(define (whole-tree-dim tree)
; (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

(define (in? dim x y)
 (and (> x (car dim)) (> y (cadr dim)) (< x (+ (car dim) (caddr dim))) (< y (+ (cadr dim) (cadddr dim)))))

; XXX reimplement in terms of addresses?
(define (utterance-parent u tree)
 (let loop ((root (whole-tree-utterance-tree tree)))
  (if (member u (utterance-args root))
   root
   (ormap loop (utterance-args root)))))
 
(define (select u tree)
 (set-selected-tree tree)
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

;(define Font #f)

(define (initialize-font)
 (set-font (ftglCreateTextureFont "/home/philip/olddesktop/vilisp/VeraMono.ttf"))
 (ftglSetFontFaceSize Font 12 72))

(define Mouse-pos (cons -1 -1))
(define Info "test")
(define Thecanvas (new my-canvas% (parent win)))

(define (rel->gl l)
 (list (car l) (- HEIGHT (+ (cadddr l) (cadr l))) (caddr l) (cadddr l)))

(define (generate-utterance-tree tree)
 (set-whole-tree-utterance-tree! tree ((v11n-node->v11n-utterance (whole-tree-v11n tree)) (whole-tree-n-tree tree) (utterance-x (whole-tree-utterance-tree tree)) (utterance-y (whole-tree-utterance-tree tree)) (if VERTICAL (node-width (whole-tree-n-tree tree) tree) 0) 0 1 tree)))

(define (find-utterance-from-laddr tree laddr)
 (if (null? laddr)
  tree
  (find-utterance-from-laddr (list-ref (utterance-args tree) (car laddr)) (cdr laddr))))

(define (update-childfuncs childfunc)
 (map (curryr set-whole-tree-childfunc! childfunc) (cdr Trees))
 (map (lambda (t) (set-whole-tree-n-tree! t (root->node (node-data (whole-tree-n-tree t)) childfunc (node-laddr (whole-tree-n-tree t))))) (cdr Trees))
 (for-each touch (map (lambda (t) (future (lambda () (generate-utterance-tree t)))) (cdr Trees)))
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
    default-horizontal-v11n
    0
    0
    1))))
 (set-whole-tree-utterance-tree! tree ((v11n-node->v11n-utterance default-horizontal-v11n) (whole-tree-n-tree tree) 0 0 0 0 '() tree))
 (set-whole-tree-selection! tree '())
 (set! Trees (append Trees (list tree)))
 tree))

(define (root->node data childlist laddr)
 (node data laddr
  (delay
   (let ((children (childlist data)))
    (map
     (lambda (arg n) (root->node arg childlist (append laddr (list n))))
     children
     (build-list (length children) values))))
  (lambda (n) (format "~s" (caddr (node-data n))))))

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
