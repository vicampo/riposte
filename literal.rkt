#lang racket/base

(provide make-literal)

(require racket/class
         racket/contract
         (only-in ejs
                  ejsexpr->string
                  ejsexpr?)
         (only-in (file "expression.rkt")
                  expression%))

(define literal%
  (class expression%
    (super-new)
    (init-field term)
    (define/override (evaluate env)
      term)
    (define/override (render)
      (ejsexpr->string term))))

(define (literal? x)
  (and (object? x)
       (is-a? x literal%)))

(define/contract (make-literal literal)
  (ejsexpr? . -> . literal?)
  (new literal%
       [term literal]))
