#lang racket

(require sgl sgl/gl)
(require "common.ss")
(require "linear-vertical-v11n.ss")

(provide other2-v11n)

(define other2-v11n
 (make-linear-vertical-v11n
  #:text-generator
   (lambda (text)
    (format "(~a" text))))

