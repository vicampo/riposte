#lang racket/base

(provide make-emptyness-expression
         make-nonemptyness-expression
         emptyness-step?)

(require racket/contract
         racket/class
         racket/match
         (only-in racket/format
                  ~a)
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string)
         (only-in racket/string
                  string-split)
         (only-in (file "./expression.rkt")
                  expression?
                  expression%)
         (only-in (file "./assertion.rkt")
                  assertion%)
         (only-in (file "./identifier.rkt")
                  identifier-expression?)
         (only-in (file "./json-pointer.rkt")
                  json-pointer-expression?)
         (only-in (file "./environment.rkt")
                  environment-has-body?
                  environment-response))

(define/contract (render-commented-out expr)
  (ejsexpr? . -> . void)
  (define rendered (ejsexpr->string expr))
  (for ([line (string-split rendered (~a #\newline))])
    (displayln (format "# ~a" line))))

(define/contract empty%
  class?
  (class assertion%
    (super-new)
    (init-field sense)
    (define/override (evaluate env)
      (match sense
        [#t
         (environment-has-body? env)]
        [#f
         (not (environment-has-body? env))])
      env)
    (define/override (render)
      (match sense
        [#f
         "is empty"]
        [#f
         "is non empty"]))))

(define/contract (emptyness-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x empty%)))

(define/contract (make-emptyness-expression)
  (-> emptyness-step?)
  (new empty%
       [sense #t]))

(define/contract (make-nonemptyness-expression)
  (-> emptyness-step?)
  (new empty%
       [sense #f]))
