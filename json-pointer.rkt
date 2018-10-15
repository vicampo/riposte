#lang racket/base

(provide json-pointer-expression?
         make-json-pointer-expression)

(require racket/class
         racket/contract
         (only-in json-pointer
                  json-pointer-value)
         (only-in (file "expression.rkt")
                  expression?
                  expression%)
         (only-in (file "environment.rkt")
                  environment-response)
         (only-in (file "util.rkt")
                  jsexpr->ejsexpr
                  ejsexpr->jsexpr))

(define json-pointer-expression%
  (class expression%
    (super-new)
    (init-field expr)
    (define/override (evaluate env)
      (define env/jsexpr (ejsexpr->jsexpr (environment-response env)))
      (json-pointer-value expr
                          env/jsexpr))
    (define/override (render)
      expr)))

(define/contract (json-pointer-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x json-pointer-expression%)))

(define/contract (make-json-pointer-expression jp)
  (string? . -> . json-pointer-expression?)
  (new json-pointer-expression%
       [expr jp]))
