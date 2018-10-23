#lang racket/base

(require setup/getinfo)

(define deps (get-info/full (current-directory)))

(module+ main
  (for [(d (deps 'deps))]
    (displayln d)))
