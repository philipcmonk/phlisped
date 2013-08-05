#lang scribble/lp

@require{docs/proofs.rkt}
@(require scribble/base)

@setup-math
@setup-proofs

@section{Summary}

The internals are fairly simple, conceptually.  Briefly, we read Racket code as a list, convert it to an ess-expression tree, convert that to an ess-manifestation tree, convert that to an ess-address tree, and then convert that to an ess-utterance tree.  Then, we display it to the screen by recursively painting the ess-utterance tree.  If input changes some aspect of the visble tree (other than perspective), then we regenerate the ess-utterance tree and paint it again.

@chunk[<*>
<require-libraries>
<define-constants>
<define-functions>
<global-variables>
<call-functions>]

@chunk[<define-constants>
<def-vertical> <def-width/height> <def-colors> <def-cellheight> <def-scrolldist> <def-filename> <def-roles>]

@chunk[<global-variables>
<def-args> <def-utterance-tree> <def-open> <def-main-dim> <def-info> <def-info-dim> <def-selection> <def-scroll-offset> <def-mouse-pos> <def-zoom-factor> <def-font> <def-cmds>]

@chunk[<define-functions>
<struct/class-declarations>
<utility-functions>
<conversion-functions>
<paint-functions>
<dimensions-functions>
<selection-functions>]

@chunk[<struct/class-declarations>
<def-ess-expr> <def-ess-man> <def-ess-addr> <def-ess-utterance> <def-my-canvas> <def-win> <def-thecanvas>]

@chunk[<conversion-functions>
<def-list->ess-expr> <def-ess-expr->ess-man> <def-ess-man->ess-addr> <def-ess-addr->ess-utterance>]

@chunk[<paint-functions>
<def-ess-utterance-paint> <def-paint-info> <def-draw-rectangle> <def-draw-text>]

@chunk[<selection-functions>
<def-go> <def-open-u> <def-close-u> <def-select>]

@chunk[<utility-functions>
<def-border> <def-get-color> <def-center> <def-ess-addr-deep-args> <def-ess-expr-code> <def-ess-utterance-parent> <def-maj/min/span-dimensions> <def-rel->gl>
<def-get-role> <def-invisible?> <def-ess-addr-args> <def-find-addr-from-laddr> <def-open?/closed?> <def-generate-utterance-tree> <def-find-utterance-from-laddr> <def-find-utterance>]

@section{Require libraries}

We import the racket/gui library for creating the window and managing the input.  We use openGL to draw everything to the screen.  We also import the ftgl c library for rendering text into openGL.  Currently, we use the bitmap font method.

@chunk[<require-libraries>
(require (except-in racket/gui yield ->))
(require (only-in racket/gui (yield yield-gui) (-> ->-gui)))
(require sgl sgl/gl)

(provide Thecanvas display-on-screen Info paint-info)

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
]

@section{Code tree}

Below we define the data structures and functions relating to the storage of the code tree.

@subsection{Global variables}

@chunk[<def-filename>
(define FILENAME "/home/philip/olddesktop/vilisp/vilisp/vilisp.ss")]

The file to be parsed.

@chunk[<def-utterance-tree>
(struct whole-tree (addr-tree childfunc utterance-tree open selection x y w h offset-x offset-y zoom) #:mutable)

(define (whole-tree-dim tree)
 (list (whole-tree-x tree) (whole-tree-y tree) (whole-tree-w tree) (whole-tree-h tree)))

(define Trees (list (apply whole-tree 
  (let* ((dummy-addr (ess-addr '(0 . "") '() (delay '())))
         (dummy-utterance (ess-utterance dummy-addr 0 0 0 0 0 0 '() (cons '(0 0 0) '(0 0 0)))))
   (list dummy-addr (lambda (a) '()) dummy-utterance (set) dummy-utterance 0 0 0 0 0 0 1)))))
(define Selected-tree (car Trees))
(define Utterance-tree #f)]

The utterance tree.  This regenerates often (see @racket[generate-utterance-tree]).

@chunk[<def-roles>
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

(define Roles ROLES)]

A hash whereby a keyword is associated with a list of length two.  In this hash, the first element is a list of ``hint'' names for the children of this function.  The second element is a function which takes the child number and is called to generate names for children after those named in the first element.

@chunk[<def-args>
(define ARGS
 '())
; (ess-man->ess-addr
;  (ess-expr->ess-man
;   (list->ess-expr
;    (call-with-input-file FILENAME
;     (lambda (f)
;      (read-accept-reader #t)
;      (define (in rem)
;       (let ((x (read rem)))
;	(if (eof-object? x)
;	 '(end)
;	 (cons x (in rem)))))
;      (in f))))
;   'root
;   '()
;   (hash))
;  (list)))]
]

The syntax tree as an @racket[ess-man].  Reads the file defined in @racket[FILENAME] and converts it.  This should be constant so long as the code does not change.

@subsection{Data structures}

The data is stored in four primary data structures:  ess-expressions, ess-manifestations, ess-addresses, and ess-utterances.  Each level is mapped onto the previous level.

Not defined here (because the declaration is implicit) is the list-address type.  This is merely a list, where the null list corresponds to the root element and each element n corresponds to the nth child (indexed from zero).  Thus, @racket['(2 0 1)] corresponds to root's third child's first child's second child.  If an address is valid in one of the trees, then if it is valid in another, the two elements must correspond (i.e. one must (directly or indirectly) map to the other).

@chunk[<def-ess-expr>
(struct ess-expr (name args type))]

Ess-expressions are the most basic representation of the code.  Each ess-expr is either an atomic datum or is defined in terms of its children.  In particular, an ess-expression has no information about its context (not even about primitive functions), and may be safely moved to a different context without worry (although I'm not sure why one would do that).

The ess-expression tree is finite and eagerly evaluated.

@racket[name] is a textual representation of the first argument.  In particular, if the first argument is not a list, then we try to convert it to a string.  Otherwise, we recursively create an ess-expression of the first argument and use its name.

@racket[args] is a list of the children of this ess-expression, or null if this is code-level.  Unless @racket[type] is @racket['list] or @racket['fun], this must be null.

@racket[type] is the type of this ess-expr.  This may be @racket['symbol], @racket['string], @racket['number], @racket['null], @racket['list], or @racket[fun] where @racket[fun] is the first element (which must be a symbol in that case).

@chunk[<def-ess-man>
'()
;(struct ess-man (expr text))]
]

Ess-manifestations are bijectively mapped onto ess-expressions (although this could be changed to a surjection without issue).  An ess-manifestation is an ess-expression plus information about its context.

The ess-manifestation tree is finite and eagerly evaluated.

@racket[expr] is the corresponding ess-expression.

@racket[text] is a textual representation of the ess-expression.  This includes the @racket[name] of the ess-expression, and it may prepend or append contextual information.  If @racket[role] is not null, then it may be prepended. If the @racket[type] of the ess-expression is @racket['define], then we append the identifier being bound.

@racket[args] is a list of the children of this ess-manifestation, or null if this is code-level.  The ess-expression of each child must correspond to a child of the ess-expression of this ess-manifestation.  In other words, the mapping from ess-manifestations to ess-expressions preserves structure.

@racket[role] is the role of this ess-manifestation, or null.  If the @racket[type] of the parent is found in @racket[ROLES], then this child is given the role found therein.

@racket[context] is a hash of all defined identifiers in the current context.  The keys are the identifier names (in symbol format), and the values are the list-addresses of the reference manifestations (i.e. the manifestation that defines the identifier).

@chunk[<def-ess-addr>
(struct ess-addr (man laddr prom-args))]

Ess-addresses are surjectively mapped onto ess-manifestations.  An ess-address is an ess-manifestation plus a list-address.  Crucially, whenever an identifer is referenced which is in the @racket[context] of the ess-manifestation, another copy of the reference implementation is created inline, so that one may merely open up the identifier to see its implementation.  In recursive definitions, this creates an infinite tree.  Thus, this tree is lazily evaluated.

The ess-address tree is usually infinite and lazily evaluated.

@racket[man] is the corresponding ess-manifestation.

@racket[laddr] is the list-address of this element.

@racket[prom-args] is a lazily generated list of children.  Either the ess-manifestation of each child must correspond to a child of the ess-manifestation of the ess-address or else this is ``symlink'', so the ess-manifestation of the only child is the reference implementation of this ess-manifestation.  In other words, the mapping from ess-addresses to ess-manifestations preserves the existing structure of the ess-manifestation, but may add on additional structure.  This is merely the promise.  Usually, one uses @racket[ess-addr-args] to force and access this.

@chunk[<def-ess-utterance>
(struct ess-utterance (addr x y w h text-w text-h args clr))]

Ess-utterances are injectively mapped onto ess-addresses.  An ess-utterance is an ess-address plus information required to draw it to the screen.

The ess-utterance tree is finite and eagerly evaluated.

@racket[addr] is the corresponding ess-address.

@racket[x] and @racket[y] are the upper left coordinates of the utterance, and @racket[w] and @racket[h] are the width and height of the utterance, and @racket[text-w] and @racket[text-h] are the width and height of the text.  These are in absolute coordinates.

@racket[args] is a list of children, or null no children should be displayed (i.e. if the ess-addr is closed or if it is both code-level and there is no expansion of this identifier).  The ess-address of each child must correspond to a child of the ess-address of this ess-utterance.  In other words, the mapping from ess-utterances to ess-addresses preserves the structure of the ess-utterance.

@racket[clr] is the color of this ess-utterance.  It is a pair where the first element is the foreground color and the second element is the background color.  This is determined by the @racket[COLORSCHEME].

@subsection{Conversion functions}

@chunk[<def-list->ess-expr> 
(define (list->ess-expr l)
 (apply
  ess-expr
  (cond
   ((symbol? l) (list (format "~s" l) '() 'symbol))
   ((string? l) (list (format "~s" l) '() 'string))
   ((number? l) (list (format "~s" l) '() 'number))
   ((null? l) (list "null" '() 'null))
   ((list? l)
    (list
     (ess-expr-name (list->ess-expr (car l)))
     (map list->ess-expr l)
     (if (symbol? (car l)) (car l) 'list)))
   (#t (list (format "~s" l) '() 'unknown)))))]

Converts a list of code into ess-expressions.  See @racket[ess-expr]

If @racket[l] is not a list, then we try to convert it into a string and store it in @racket[name].  Currently supported are symbols, strings, numbers, and null.  If it is a list, then we recursively create an ess-expression of the first argument and use its name.  If we don't recoginze the type, then we use @racket[format] to convert it to a string type.

If @racket[l] is not a list, then @racket[args] is null.  Else, @racket[args] is the recursive map of the elements of @racket[l].

@racket[type] is defined as follows:

@itemize[
@item{If @racket[l] is symbol, then @racket['symbol].}
@item{If @racket[l] is string then @racket['string].}
@item{If @racket[l] is number then @racket['number].}
@item{If @racket[l] is null, then @racket['null].}
@item{If @racket[l] is list, then if first element is symbol, then that symbol, else @racket['list].}
@item{If @racket[l] is other type, then @racket['unknown].}]

Runs in linear time with the number of descendants of @racket[l], that is, the length of the code.

@chunk[<def-ess-expr->ess-man>
'()
;(define (ess-expr->ess-man expr role laddr context)
; (ess-man
;  expr
;  (string-append
;   (if (null? role) "" (string-append (symbol->string role) ":  "))
;   (ess-expr-name expr)
;   (if (eq? (ess-expr-type expr) 'define) (string-append ":  " (ess-expr-name (cadr (ess-expr-args expr)))) ""))
;  (let ((new-context
;	 (apply hash-set* context
;	  (apply append
;	   (map
;	    (lambda (arg)
;	     (let ((id (cadr (ess-expr-args (car arg)))))
;	      (list (string->symbol (ess-expr-name id)) (append laddr (cdr arg)))))
;	    (filter
;	     (lambda (arg) (eq? (ess-expr-type (car arg)) 'define))
;	     (map list (ess-expr-args expr) (build-list (length (ess-expr-args expr)) values))))))))
;   (map
;    (lambda (arg n)
;     (ess-expr->ess-man
;      arg
;      (get-role (ess-expr-type expr) n)
;      (append laddr (list n))
;      new-context))
;    (ess-expr-args expr)
;    (build-list (length (ess-expr-args expr)) values)))
;  role
;  context))]
]

Converts an ess-expression into an ess-manifestation.

@racket[expr], @racket[role], and @racket[context] are passed as-is to the constructor of the ess-manifestation.

For @racket[text], we have the @racket[name] of @racket[expr] prepended with the role (if not null) and appended with the identifier if @racket[type] is @racket['define].

When recursively converting the children of @racket[expr], we first generate the new context by checking if each child is of @racket[type] @racket['define].  For each one, we add an entry to the context for the children.  Then, we recursively map ourselves onto the children, updating the address and role fro each one.

Runs in linear time with the number of descendants of @racket[expr].

@chunk[<def-ess-man->ess-addr>
'()
;(define (ess-man->ess-addr man laddr)
; (ess-addr man laddr
;  (delay
;   (if (and
;	(eq? (ess-expr-type (ess-man-expr man)) 'symbol)
;	(not (eq? (ess-man-role man) 'identifier))
;	(not (eq? (ess-man-role (ess-addr-man (find-addr-from-laddr ARGS (drop-right laddr 1)))) 'identifier))
;	(hash-has-key? (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
;    (let* ((ref-laddr (hash-ref (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
;	   (ref-addr (find-addr-from-laddr ARGS ref-laddr)))
;     (list (ess-man->ess-addr
;	    (ess-addr-man ref-addr)
;	    (append laddr (list 0)))))
;    (map
;     (lambda (arg n) (ess-man->ess-addr arg (append laddr (list n))))
;     (ess-man-args man)
;     (build-list (length (ess-man-args man)) values))))))]
]

Converts an ess-manifestation into an ess-address.

@racket[man] and @racket[laddr] are passed as-is to the constructor of the ess-address.

We recursively, but lazily, convert each of the children of @racket[man].  Alternatively, if we are at code level, then we let @racket[args] be the reference implementation of the given ess-manifestation.

Runs in constant time (remember that it does not eagerly generate children).

@chunk[<def-ess-addr->ess-utterance>
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
        (if (zero? col) COLOR6 (if (odd? col) COLOR3 COLOR4))))))))))]

Converts an ess-address to an ess-utterance.

@racket[addr], @racket[x], and @racket[y] are passed as-is to the constructor of the ess-utterance.

For the width, if @racket[VERTICAL] is true, then we use @racket[w].  Else we use the max of the width of @racket[text] and the sum of the widths of each of the children.

For the height, we call @racket[ess-addr-height].

For the @racket[text-w], we use the width of @racket[text].

For the @racket[text-h], we use the height of @racket[text].

For the @racket[color], we call @racket[get-color].

For the @racket[args], we recursively ``map'' ourselves onto the children of @racket[addr].  It is not a true map since the major dimension of each child is determined by the sum of the major dimension spans of all previous children.  Additionally, if @racket[VERTICAL] is true, then the width of each child is determined by the maximum width of any child.

@subsection{Utility functions}

@chunk[<def-ess-addr-args>
(define ess-addr-args (compose force ess-addr-prom-args))]

Forces the generation of the children of @racket[addr].

Runs in constant time except the first time, when it runs in linear time relative to the number of children.  See @racket[ess-man->ess-addr] to see what is being forced.

@chunk[<def-ess-addr-deep-args>
(define (ess-addr-deep-args addr pred)
 (cons addr (if (pred addr) '() (map (lambda (a) (ess-addr-deep-args a pred)) (ess-addr-args addr)))))]

Returns a deep list of the children of @racket[addr], recursing until @racket[pred] is false.

Runs, technically, in amortized linear time relative to the number of descendants of @racket[addr].  In practice, @racket[addr] often has an infinite number of descendants, so it is important to ensure @racket[pred] will eventually return false.  This runs, then, in linear time relative to the number of descendants of @racket[addr] until @racket[pred] is false.

Note that this references @racket[ess-addr-args], so if these have not yet been generated, they will be generated.

@chunk[<def-ess-expr-code>
'()
;(define ess-expr-code ess-man-code)
; (if (member (ess-expr-type expr) '(string symbol number unknown))
;  (ess-expr-name expr)
;  (string-append "(" (if (eq? (ess-expr-type expr) 'list) (ess-expr-name expr) "") (string-join (map ess-expr-code (ess-expr-args expr)) " ") ")")))]
]

Gets a string representation of the code of an ess-expression.

Runs in linear time relative to the number of descendants of the ess-expression.

@chunk[<def-get-role>
(define (get-role type n)
 (if (hash-has-key? Roles type)
  (let ((r (hash-ref Roles type)))
   (if (< n (length (car r)))
    (list-ref (car r) n)
    (if (null? (cadr r))
     '()
     ((cadr r) (- n (length (car r)))))))
  '()))]

Returns the role of the @racket[n]th child of an element of @racket[type].  Found by looking up in @racket[Roles].

Runs in constant time (or, rarely, the complexity class of the function found in Roles).

@chunk[<def-find-addr-from-laddr>
(define (find-addr-from-laddr addr laddr)
 (if (null? laddr)
  addr
  (find-addr-from-laddr (list-ref (ess-addr-args addr) (car laddr)) (cdr laddr))))]

Returns the ess-address at the given list-address.

Runs in linear time relative to the length of @racket[laddr].

@chunk[<def-find-utterance-from-laddr>
(define (find-utterance-from-laddr tree laddr)
 (if (null? laddr)
  tree
  (find-utterance-from-laddr (list-ref (ess-utterance-args tree) (car laddr)) (cdr laddr))))]

@chunk[<def-ess-utterance-parent>
(define (ess-utterance-parent u tree)
 (apply find-utterance (whole-tree-utterance-tree tree)
  (if VERTICAL
   (list (+ (ess-utterance-x u) -1) (ess-utterance-y u) tree)
   (list (ess-utterance-x u) (+ (ess-utterance-y u) -1) tree))))]

Returns the parent of @racket[u].

Internally, we find the parent by identifying the utterance one absolute pixel in the major dimension from @racket[u].

Runs in the same complexity class as find-utterance, which is worst-case linear, but average-case logarithmic relative to the size of the utterance tree.

@chunk[<def-generate-utterance-tree>
(define (generate-utterance-tree tree)
    (set-whole-tree-utterance-tree! tree (ess-addr->ess-utterance (whole-tree-addr-tree tree) (ess-utterance-x (whole-tree-utterance-tree tree)) (ess-utterance-y (whole-tree-utterance-tree tree)) (if VERTICAL (ess-addr-width (whole-tree-addr-tree tree) tree) -1) 0 1 tree))
    (set-whole-tree-selection! tree (find-utterance-from-laddr (whole-tree-utterance-tree tree) (ess-addr-laddr (ess-utterance-addr (whole-tree-selection tree))))))]

Regenerates the utterance tree from @racket[addr].

Runs in the complexity class of @racket[ess-addr->ess-utterance].

@section{Display}

Below we define the data structures and functions relating to the display of the the code tree.

@subsection{Global variables}

@chunk[<def-vertical>
(define VERTICAL #f)]

If true, then the major dimension is vertical, and the tree opens left-to-right.  Else, the major dimension is horizontal, and the tree opens top-to-bottom.  This is not actually a constant (it can be changed by pressing the ``f'' key).

Rigorously, if @racket{VERTICAL} is true, then y is the major dimension, h is the span over the major dimension, x is the minor dimension, and w is the span over the minor dimension.  If @racket{VERTICAL} is false, then x is the major dimension, w is the span over the major dimension, y is the minor dimension, and h is the span over the minor dimension.

@chunk[<def-width/height>
(define WIDTH (* 1 1600))
(define HEIGHT 899)]

Defines the default width and height of the window.

@chunk[<def-colors>
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
(define FGCOLOR '(255 255 255))]

All colors are defined as a pair where the first element is the foreground color and the second element is the background color.

@racket[COLORSCHEME] may be @racket['alternate] or @racket['gradient], and can be changed by pressing ``F''.  The gradient color scheme causes each set of siblings to be colored on a gradient starting from @racket[INITIALCOLOR] and changing by the end by the delta in @racket[COLORRANGES].  The gradient color scheme is currently inoperative.

For the alternate color scheme, we have the following logic to determine the color of a cell:

@itemize[
@item{If code-level (i.e. if this is the literal code in the file -- the ``lowest level''), then
@itemize[
	@item{if eldest child, then @racket[CODECOLOR3],}
	@item{if odd-numbered child, then @racket[CODECOLOR1],}
	@item{if even-numbered child, then @racket[CODECOLOR2].}]}

@item{If in odd-numbered row, then
@itemize[
	@item{if eldest child, then @racket[COLOR5],}
	@item{if odd-numbered child, then @racket[COLOR1],}
	@item{if even-numbered child, then @racket[COLOR2].}]}

@item{if in even-numbered row, then
@itemize[
	@item{if eldest child, then @racket[COLOR6],}
	@item{if odd-numbered child, then @racket[COLOR3],}
	@item{if even-numbered child, then @racket[COLOR4].}]}
]

@racket[SELCOLOR] is the color used for coloring the selected cell in either mode.

@racket[INFOCOLOR] is the color of the sidebar on the bottom.

@racket[BGCOLOR] is the background color (i.e. the color of everything not part of the tree) in either mode.

I honestly have no idea what @racket[FGCOLOR] is supposed to do, but it seems to be related to the gradient mode.

@chunk[<def-cellheight>
(define CELLHEIGHT 25)]

The height of a cell (in absolute coordinates) in horizontal mode.  We don't just take the height of text because we may want to add some padding.

@chunk[<def-open>
'()
;(define Open (set))]
]

The set of open @racket[ess-addr]s.  If an @racket[ess-addr] is listed here, then its children are visible (i.e. are part of @racket[Utterance-tree]).  See @racket[open?] and @racket[closed?].

@chunk[<def-main-dim>
'()
;(define Main-dim (list 0 0 (round (/ (* 2 WIDTH) 3)) (- HEIGHT 30)))
;(define Side-dim (list (round (/ (* 2 WIDTH) 3)) 0 (round (/ WIDTH 3)) (- HEIGHT 30)))]
]

The x,y,w,h (in relative coordinates) of the main viewing area.

@chunk[<def-info-dim>
(define Info-dim (list 0 (- HEIGHT 30) WIDTH 30))]

The x,y,w,h (in relative coordinates) of the info sidebar.  Defaults to the bottom 30 pixels.

@chunk[<def-info>
(define Info "test")]

The text to be displayed in the info sidebar.

@chunk[<def-selection>
'()
;(define Selection (ess-utterance ARGS 0 0 0 0 0 0 '() #f))]
]

The currently selected utterance.  This is colored according to @racket[SELCOLOR] and many input events act on this utterance.  The selection is changed by some input events.

@chunk[<def-scroll-offset>
'()
;(define Scroll-offset-x 0)
;(define Scroll-offset-y 0)
]

The scroll offsets, in absolute coordinates.  If the offsets for x and y are a and b, then the upper left corner of the viewport is at (-a,-b).  This is primarily changed by the scrollwheel, but zooming can also change this.

@chunk[<def-zoom-factor>
'()
;(define Zoom-factor 1)]
]

The zoom-factor, i.e. the ratio of change in relative coordinates to change in absolute coordinates.  Thus, if it is 1/2, then we are zoomed out so that everything is half as tall and half as wide.

@chunk[<def-font>
(define Font (ftglCreateBitmapFont "/home/philip/olddesktop/vilisp/VeraMono.ttf"))
(ftglSetFontFaceSize Font 12 72)]

The font used for all text.  Two things are important:  the type of font, and the font itself.  The font file is a truetype font file.  The type of font is one of those allowed by FTGL.  We use Bitmap since it seems to have the best performance.  Pixmap is  a little slower but is a better rendering.  The others do not seem to work as-is.

@chunk[<def-win>
(define win (new frame% (label "vilisp") (min-width WIDTH) (min-height HEIGHT)))]

@chunk[<def-thecanvas>
(define Thecanvas (new my-canvas% (parent win)))]

@subsection{Data structures}

@chunk[<def-my-canvas>
(define (resize w h)
 (gl-viewport 0 0 w h)
 #t)

<def-syntax-mouse-handler>

(define-syntax (with stx)
 (let* ((l (syntax->datum stx))
        (body (cadr l))
        (defs (cddr l))
        (lams (map (lambda (def) `(,(car def) (lambda ,(cadr def) ,@(cddr def)))) defs)))
  (datum->syntax stx `(letrec ,lams ,@body))))

(define my-canvas%
 (with
  ((class* canvas% ()
   (inherit with-gl-context swap-gl-buffers)
 
   <def-paint-handler>
   <def-mouse-input>
   <def-key-input>
 
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
  ))]

The racket side of the openGL canvas.  Handles painting and mouse/keyboard input.

@subsection{Paint functions}

@chunk[<def-paint-handler>
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
]

Resets the openGL buffer, sets the view, and calls @racket[ess-utterance-paint] with @racket[Utterance-tree].

Runs in the computational complexity of @racket[ess-utterance-paint].

@chunk[<def-ess-utterance-paint> 
(define paintit (lambda () '()))


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

(define (root->ess-addr man childlist laddr)
 (ess-addr man laddr
  (delay
;   (if (and
;	(eq? (ess-expr-type (ess-man-expr man)) 'symbol)
;	(not (eq? (ess-man-role man) 'identifier))
;	(not (eq? (ess-man-role (ess-addr-man (find-addr-from-laddr ARGS (drop-right laddr 1)))) 'identifier))
;	(hash-has-key? (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
;    (let* ((ref-laddr (hash-ref (ess-man-context man) (string->symbol (ess-expr-name (ess-man-expr man)))))
;	   (ref-addr (find-addr-from-laddr ARGS ref-laddr)))
;     (list (root->ess-addr
;	    (ess-addr-man ref-addr)
;	    (append laddr (list 0)))))
    (let ((children (childlist man)))
     (map
      (lambda (arg n) (root->ess-addr arg childlist (append laddr (list n))))
      children
      (build-list (length children) values))))))

(define (ess-man-text lst)
 (format "~s" (cdr lst)))
; (if (list? lst)
;  (ess-man-text (car lst))
;  (format "~s" lst)))
 
]

Checks if an ess-utterance is visible, and if so, paints it and recursively maps onto its children.  Drawing an ess-utterance means drawing a rectangle with attributes defined in the ess-utterance, and then drawing text on top of it.  Only draws text if not zoomed out further than natural (@racket[Zoom-factor] of @racket[1]).

Runs in linear time (with a fairly high constant) relative to the number of visible ess-utterances (which may be very large if zoomed out far).

@chunk[<def-paint-info>
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
 (ftglRenderFont Font (substring text 0 (min (string-length text) 120)) 65535)
 (if swap
  (send Thecanvas swap-gl-buffers)
  '())
  (gl-disable 'scissor-test))]

Paints information into the display at the bottom of the screen.  Usually, this is used for displaying the code of the hovered-over utterance.

Runs in constant time (I believe; it may run in linear time relative to the length of the text).

@chunk[<def-draw-rectangle>
'()
]

Draws a rectangle at @racket[(x y)] with width @racket[w] and height @racket[h], in absolute coordinates.  Uses the rgb values specified in the 3-list @racket[color].

Runs in constant time.

@chunk[<def-draw-text>
'()
]

Draws text at @racket[(x y)], in absolute coordinates.  Uses the rgb values specified in the 3-list @racket[color].

Runs in constant time (I believe; it may run in linear time relative to the length of the text).

@subsection{Selection functions}

@chunk[<def-go>
'()]

Changes the selection in the direction of @racket[dir], which may be one of @racket['left], @racket['down], @racket['up], and @racket['right].  Calls @racket[select] to actually set the variable.  Regenerates the utterance tree.

Runs in the computational complexity of @racket[generate-utterance-tree].

@chunk[<def-open-u>
'()]

Opens the ess-utterance.  If @racket[deep?], then opens all descendants down to the next code level.  If not @racket[deep?], its immediate children are opened.  If these are already open, then the next level is opened, etc.  This was particularly late night coding -- don't ask me how the code works.  Regenerates the utterance tree.

Runs in the computational complexity of @racket[generate-utterance-tree] except in cases where very large ess-utterances are being opened, when it may run in linear time relative to the number of already-open descendants (although I suspect it is always dominated by @racket[generate-utterance-tree]).

@chunk[<def-close-u>
'()]

Closes the ess-utterance.  If @racket[deep?], then closes all descendants.  If not @racket[deep?], then close only the deepest layer.  In either case, if all descendants are already closed, then closes the parent and selects the parent.  Regenerates the utterance tree.

Runs in the computational complexity of @racket[generate-utterance-tree] except in cases where very large ess-utterances are being closed, when it may run in linear time relative to the number of already-open descendants (although I suspect it is always dominated by @racket[generate-utterance-tree]).

@chunk[<def-select>
(define (select u tree)
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
   '())))]

Selects the ess-utterance.  Sets @racket[Selection] to the ess-utterance, and if invisible, then centers viewport onto the new selection.  Does @emph{not} regenerate the utterance tree -- this must be done by the caller, or else the change will not be visible.

Runs in constant time.

@subsection{Dimensions functions}

@chunk[<dimensions-functions>
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
]

@subsection{Utility functions}

@chunk[<def-border>
(define (border?) (eq? COLORSCHEME 'gradient))]

Returns true if we are using a color scheme that requires a border.

Runs in constant time.

@chunk[<def-get-color>
'()]

@racket[addr] is an ess-address, and @racket[siblings] is the number of siblings it has.  This returns the color of this ess-address, as determined by @racket[COLORSCHEME].

Runs in constant time.

@chunk[<def-center>
'()
]

@racket[offset] is the starting point of a line segment.

@racket[lenwhole] is the length of the line segment.

@racket[lenpiece] is the length of a smaller line segment that is entirely in the visible portion of the larger line segment, and subject to this, it is centered relative to the visible portion of the larger line segment.

@racket[start] is the starting edge of the viewport.

@racket[width] is the size of the viewport.

Informally, we return the required starting point for the smaller line segment to be centered within the visible portion of the larger line segment.

We assume that @${lenpiece \le lenwhole} and @${lenpiece \le width}.

Runs in constant time.

@theorem{If @${L = [offset,offset+lenwhole]} and @${V = [start,start+width]}, then @racket[center] is such that @${l = [center,center+lenpiece] \subseteq L}, and subject to this @${l \cap V} is maximum, and if @${l \subseteq V}, then @${center + \frac{1}{2} lenpiece = \max\{offset,start\} + \frac{1}{2} width}.}

@proof{First, note that @$${visiblewidth = \min\{offset+lenwhole,start+width\} - \max\{offset,start\}} is the width of @${L \cap V}.  We have two cases.

@itemize[
@item{If @${visiblewidth < lenpiece}, then @${l \not\subseteq V}.  Since @${offset+lenwhole \ge center + lenpiece}, we have @${center \le offset + lenwhole - lenpiece}.  Also, @${center \ge offset}.  Then, we have two cases.
@itemize[
	@item{If @${offset < start}, then since the size of @${l \cap V} increases as @${center} increases, we let @${center = offset + lenwhole - lenpiece}.}

	@item{If @${offset \ge start}, then since the size of @${l \cap V} decreases as @${center} increases, we let @${center = offset}.}]}

@item{If @${visiblewidth \ge lenpiece}, then @${l \subseteq V}.  Then, since @${center + \frac{1}{2} lenpiece = \max\{offset,start\} + \frac{1}{2} width}, we have @${center = \max\{offset,start\} + \frac{1}{2} width - \frac{1}{2} lenpiece}.}]}

@chunk[<def-maj/min/span-dimensions>
(define (ess-utterance-maj-dim u) (if VERTICAL (ess-utterance-y u) (ess-utterance-x u)))
(define (ess-utterance-maj-dim-span u) (if VERTICAL (ess-utterance-h u) (ess-utterance-w u)))
(define (ess-utterance-min-dim u) (if VERTICAL (ess-utterance-x u) (ess-utterance-y u)))
(define (ess-utterance-min-dim-span u) (if VERTICAL (ess-utterance-w u) (ess-utterance-h u)))
(define (maj-dim x y) (if VERTICAL y x))
(define (min-dim x y) (if VERTICAL x y))]

Returns the value in a particular dimension, as defined by @racket{VERTICAL}.

Runs in constant time.

@chunk[<def-rel->gl>
(define (rel->gl l)
 (list (car l) (- HEIGHT (+ (cadddr l) (cadr l))) (caddr l) (cadddr l)))]

Converts relative coordintes into OpenGL relative coordinates (deals with y-dimension weirdness).

Runs in constant time.

@chunk[<def-invisible?>

(define (in? dim x y)
 (and (> x (car dim)) (> y (cadr dim)) (< x (+ (car dim) (caddr dim))) (< y (+ (cadr dim) (cadddr dim)))))]

Returns true if the rectangle is visible, or if its children may be visible.

Runs in constant time.

@chunk[<def-open?/closed?>
(define (open? addr tree)
 (set-member? (whole-tree-open tree) addr))
; #t)
(define closed? (negate open?))]

Predicates to check if an ess-address is open (i.e. it is in @racket[Open]).

Runs in constant time.

@chunk[<def-find-utterance>
(define (find-utterance root x y tree)
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
   (ess-utterance-args root))))]

@subsection{newstuff}

@chunk[<def-cmds>
(define cmds (hash))

(struct win-word (paint))
(struct wout-word (paint w))

(define-syntax (def-wout-cmd stx)
 (let* ((l (syntax->datum stx))
	(name (cadr l))
	(body (cddr l)))
   (datum->syntax stx `(set! cmds (hash-set cmds ,name (lambda (args) ,@body))))))

(define-syntax (def-win-cmd stx)
 (let* ((l (syntax->datum stx))
	(name (cadr l))
	(body (cddr l)))
   (datum->syntax stx `(set! cmds (hash-set cmds ,name (lambda (args) (win-word (lambda (x y w) ,@body))))))))

;(def-win-cmd 'chunk
; (ess-utterance-paint 
;  (ess-addr->ess-utterance
;   (ess-man->ess-addr
;    (ess-expr->ess-man
;     (list->ess-expr args)
;     'root
;     '()
;     (hash))
;    (list))
;   x y w 0 1)))

; (draw-text (string-append "code:  " (format "~s" args) "\n") x y (car INFOCOLOR)))

;(def-wout-cmd 'section
; (map 
;  (lambda (n)
;   (let ((text (format "~s" args)))
;    (wout-word
;     (lambda (x y)
;      (draw-text text x y (car INFOCOLOR)))
;     (box-width text))))
;  '(1 2 3 4 5)))
;
;(def-wout-cmd 'subsection
; (map 
;  (lambda (n)
;   (let ((text (format "~s" args)))
;    (wout-word
;     (lambda (x y)
;      (draw-text text x y (car INFOCOLOR)))
;     (box-width text))))
;  '(1 2 3)))
;
;(def-win-cmd 'itemize
; (draw-text (string-append "itemize:  " (format "~s" args) "\n") x y (car INFOCOLOR)))
;
(def-wout-cmd 'text
 (map 
  (lambda (arg)
   (wout-word
    (lambda (x y)
;     (draw-text arg x y (car INFOCOLOR)))
      (newline))
    (+ 10 (box-width arg))))
  (regexp-split #px" " (format "~s" args))))

(def-wout-cmd 'unknown
 (map 
  (lambda (n)
   (let ((text (string-append "unknown:  " (format "~s" args) "\n")))
    (wout-word
     (lambda (x y)
;      (draw-text text x y (car INFOCOLOR)))
      (newline))
     (box-width text))))
  '(1)))


(define (list->words l)
 (flatten
  (map
   (lambda (itm)
    (cond
     ((string? itm)
      ((hash-ref cmds 'text) itm))
     ((and (list? itm) (hash-has-key? cmds (car itm)))
      ((hash-ref cmds (car itm)) itm))
     (#t
      ((hash-ref cmds 'unknown) itm))))
   l)))

(define WORDS 
 (list->words
  (cdddar
   (cdddar
    (call-with-input-file FILENAME
     (lambda (f)
      (read-accept-reader #t)
      (define (in rem)
       (let ((x (read rem)))
	(if (eof-object? x)
	 '(end)
	 (cons x (in rem)))))
      (in f)))))))

(define (fork op a b)
 (cons (op (car a) (car b)) (op (cdr a) (cdr b))))

(define (word-paint words x y cx cy w h)
 (let ((word (car words)))
  (if (> cy h)
   (cons cx cy)
    (if (win-word? word)
     (begin
      ((win-word-paint word) x (+ 30 cy) w)
      (word-paint (cdr words) x y x (+ 60 cy) w h))
     (if (> (+ cx (wout-word-w word)) w)
      (let ((xx x) (yy (+ 30 cy)))
       ((wout-word-paint word) xx yy)
       (word-paint (cdr words) x y (+ xx (wout-word-w word)) yy w h))
      (let ((xx cx) (yy cy))
       ((wout-word-paint word) xx yy)
       (word-paint (cdr words) x y (+ xx (wout-word-w word)) yy w h)))))))
]

@section{Input}

Below we define the functions relating to input from the user.

@chunk[<def-scrolldist>
(define SCROLLDIST 100)]

@chunk[<def-mouse-pos>
(define Mouse-pos (cons -1 -1))]

The last-recorded position of the mouse, in relative coordinates.  Used for dragging.

The distance traversed by a single mouse wheel movement event.  Currently, this is in absolute coordinates, but that may change.

@chunk[<def-syntax-mouse-handler>
<def-mouse-handler-hash> <def-mouse-handler-syntax>]

@chunk[<def-mouse-handler-syntax>
(define-syntax (define-mouse-handler stx)
 (let* ((args (syntax->datum stx))
	(reqs (cadr args))
	(code (cddr args))
	(vals (map (lambda (req) (list req (hash-ref mouse-handler-hash req))) reqs)))
  (datum->syntax stx `(let ((tree (let ((x (send event get-x)) (y (send event get-y))) (or (findf (compose (curryr in? x (- HEIGHT y)) whole-tree-dim) Trees) Selected-tree))))
                       (let ,vals ,@code)))))]

This is a convenience macro for writing responses to mouse events.  Its usage is as follows:  @racket[(define-mouse-handler (data) code)].  @racket[(data)] is a list of symbols to be bound to their meaning.  For example, @racket['clicked] is bound to the ess-utterance that was clicked on.  For a list of all possible symbols, see @racket[mouse-handler-hash] (below).  @racket[code] is the actual handler.  This may refer to any of the symbols in @racket[data] (since they have now been bound).

Note that this is dependent (through @racket[mouse-handler-hash]) on the mouse event being named ``@racket[event]''.

@chunk[<def-mouse-handler-hash>
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
			'rel-y '(send event get-y)))]

This is the hash for @racket[define-mouse-handler].  Keys are symbols and values are the code needed to calculate their value.

@chunk[<def-mouse-input>
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
    (#t '())))]

@chunk[<def-key-input>
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
     (normalize-trees)
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
     (send this on-paint))))]

@section{Preparados, listos, ya}

Enough talk.

@chunk[<call-functions>
(send win show #t)
]

