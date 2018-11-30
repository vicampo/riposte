#lang racket/base

(require setup/getinfo
         racket/match)

(define deps (get-info/full (current-directory)))

(module+ main
  (for [(d (deps 'deps))]
    (displayln
     (match d
       [(? string?)
        d]
       [(list (? string? p) #:version whatever)
        p]))))
