#lang racket

(require sgl sgl/gl "../core/common.rkt" "helpers/linear-vertical-v11n.rkt")
(provide visualization)
(define visualization (letrec ((v60 (lambda (v64) (format "//~a" v64))) (v19 (lambda (v52 v53 v54 v55 v56 v57) (letrec ((v51 v56) (v50 v55) (v49 v54) (v48 v53)) (draw-rectangle (letrec ((v42 v52)) (if (letrec ((v27 v57)) (eq? Selected-tree v27)) (letrec ((v31 -)) (cdr v42)) (map (curryr / 3) (cdr v42)))) v48 v49 v50 v51))))) (make-linear-vertical-v11n #:text-generator v60)))
