#lang racket/base

(provide make-echo-step
         echo-step?)

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
         (only-in (file "./step.rkt")
                  step%)
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

(define/contract echo%
  class?
  (class step%
    (super-new)
    (init-field expr)
    (define/override (evaluate env)
      (match expr
        [#f
         (cond [(environment-has-body? env)
                (render-commented-out (environment-response env))]
               [else
                (displayln "# (empty response)")])]
        [(? expression?)
         (render-commented-out (send expr evalutate env))])
      env)
    (define/override (render)
      (cond [(eq? #f expr)
             (format "echo")]
            [(expression? expr)
             (format "echo ~a" (send expr render))]))))

(define/contract (echo-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x echo%)))

(define/contract (make-echo-step e)
  ((or/c false/c identifier-expression? json-pointer-expression?) . -> . echo-step?)
  (new echo%
       [expr e]))
