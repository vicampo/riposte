#lang racket/base

(provide start)

(define (start req)
  (response/json #:code 204))
