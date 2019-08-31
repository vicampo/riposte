#lang racket/base

(provide json-object?
         json-array?)

(require json)

; it is assumed that x is a jsexpr? value
(define (json-object? x)
  (hash? x))

; it is assumed that x is a jsexpr? value
(define (json-array? x)
  (list? x))
