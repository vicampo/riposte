#lang racket/base

(provide param-environment
         param-cwd)

(require (only-in (file "environment.rkt")
                  make-fresh-environment))

;; the environment under which we're evaluating things
(define param-environment
  (make-parameter (make-fresh-environment)))

(define param-cwd
  (make-parameter #f))
