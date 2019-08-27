#lang racket/base

(provide json-object?)

(require json)

(define (json-object? x)
  (and (jsexpr? x)
       (hash? x)))
