#lang racket/base

(require (file "common.rkt"))

(define (start req)
  (response/jsexpr (list) #:code 299))

(module+ main
  (run start))
