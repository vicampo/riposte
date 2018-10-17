#lang racket/base

(provide json-pointer-expression?
         make-json-pointer-expression)

(require racket/class
         racket/contract
         (only-in json-pointer
                  json-pointer-value)
         (only-in ejs
                  ejsexpr?
                  ejs-object?
                  ejs-array?)
         (only-in (file "expression.rkt")
                  expression?
                  expression%)
         (only-in (file "environment.rkt")
                  environment-response))

(define json-pointer-expression%
  (class expression%
    (super-new)
    (init-field expr)
    (define/override (evaluate env)
      (define res (environment-response env))
      (unless (ejsexpr? res)
        (error "Respond body is either missing or is malformed JSON."))
      (when (and (not (ejs-object? res))
                 (not (ejs-array? res)))
        (error "Respond is well-formed JSON, but is neither an array nor an object."))
      (json-pointer-value expr res))
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
