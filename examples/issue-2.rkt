#lang racket/base

(require (file "common.rkt"))

(define (foo/bar req)
  (response/empty))

(define-values (start url-generator)
  (dispatch-rules
   [("foo" "bar") #:method "get" foo/bar]
   [("foo" "bar") #:method (regexp ".*") not-allowed]
   [else not-found]))

(module+ main
  (run start))
