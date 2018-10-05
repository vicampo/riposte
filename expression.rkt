#lang racket/base

(provide expression?
         expression%)

(require racket/class
         racket/contract
         racket/match
         racket/system
         racket/list
         racket/hash
         ejs
         json-pointer
         (file "environment.rkt"))

(define expression%
  (class object%
    (class/c [evaluate (->m environment? ejsexpr?)])
    (super-new)
    (abstract evaluate
              render)))

(define/contract (expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x expression%)))
