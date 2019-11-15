#lang racket/base

(require (file "common.rkt"))

(define (foo req)
  (response/jsexpr (hash 'foo "bar")))

(define (bar req)
  (response/jsexpr (hash 'bar "chocolate")))

(define-values (start url-generator)
  (dispatch-rules
   [("foo") #:method "get" foo]
   [("bar") #:method "get" bar]))

(module+ main
  (run start))
