#lang racket/base

(require (file "common.rkt"))

(define (start req)
  (response/jsexpr (hash 'foo 4) #:code 201))

(module+ main
  (run start))
