#lang racket/base

(provide value?)

(require (only-in ejs
                  ejsexpr?))

(define (value? x)
  (ejsexpr? x))
