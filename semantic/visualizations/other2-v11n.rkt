#lang racket

(require "helpers/linear-vertical-v11n.ss")
(provide other2-v11n)
(define other2-v11n (letrec ((v7 (lambda (a14) (letrec ((v12 a14)) (format "//~a" v12))))) (make-linear-vertical-v11n #:text-generator v7)))