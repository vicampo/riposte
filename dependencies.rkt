#lang racket/base

(require setup/getinfo)

(define deps (get-info (list "riposte")))

(module+ main
  (for [(d (deps 'deps))]
    (displayln d)))
