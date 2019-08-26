#lang racket/base

(require (file "common.rkt"))

(define (start req)
  (response/empty #:code 204))

(module+ main
  (run start))
