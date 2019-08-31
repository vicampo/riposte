#lang racket/base

(require (file "common.rkt"))

(define (foo req)
  (response/jsexpr (hash 'foo 4)))

(define (bar req)
  (response/jsexpr (hash 'bar "hi!")))

(define-values (start url-generator)
  (dispatch-rules
   [("foo") #:method "post" foo]
   [("bar") #:method "post" bar]))

(module+ main
  (run start))
