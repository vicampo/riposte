#lang racket/base

(provide identifier-expression?
         variable-identifier-expression?
         parameter-identifier-expression?
         header-identifier-expression?
         make-environment-variable-identifier-expression
         make-variable-identifier-expression
         make-parameter-identifier-expression
         make-header-identifier-expression
         make-response-code-identifier)

(require racket/class
         racket/contract
         (only-in racket/system
                  string-no-nuls?)
         (file "expression.rkt")
         (only-in (file "environment.rkt")
                  lookup-variable
                  lookup-header
                  lookup-global-variable))

(define identifier-expression%
  (class expression%
    (super-new)
    (init-field name)
    (define/public (get-name)
      name)))

(define/contract (identifier-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x identifier-expression%)))

(define variable-identifier-expression%
  (class identifier-expression%
    (super-new)
    (inherit-field name)
    (define/override (evaluate env)
      (lookup-variable name env))
    (define/override (render)
      (format "$~a" name))))

(define/contract (variable-identifier-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x variable-identifier-expression%)))

(define header-identifier-expression%
  (class identifier-expression%
    (super-new)
    (inherit-field name)
    (define/override (evaluate env)
      (lookup-header name env))
    (define/override (render)
      (format "^~a" name))))

(define/contract (header-identifier-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x header-identifier-expression%)))

(define environment-variable-identifier-expression%
  (class identifier-expression%
    (super-new)
    (inherit-field name)
    (init-field fallback)
    (define/override (evaluate env)
      (define from-env (getenv name))
      (cond [(string-no-nuls? from-env)
             from-env]
            [(string? fallback)
             fallback]
            [else
             (error (format "Environment variable ~a missing from the environment, and no fallback was provided." name))]))
    (define/override (render)
      (cond [(string? fallback)
             (format "@~a with fallback" name fallback)]
            [else
             (format "@~a" name)]))))

(define parameter-identifier-expression%
  (class identifier-expression%
    (super-new)
    (inherit-field name)
    (define/override (evaluate env)
      (lookup-global-variable env name))
    (define/override (render)
      (format "%~a" name))))

(define/contract (parameter-identifier-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x parameter-identifier-expression%)))

(define/contract (environment-variable-identifier-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x environment-variable-identifier-expression%)))

(define/contract (make-header-identifier-expression n)
  (string? . -> . header-identifier-expression?)
  (new header-identifier-expression%
       [name n]))

(define/contract (make-variable-identifier-expression n)
  (string? . -> . variable-identifier-expression?)
  (new variable-identifier-expression%
       [name n]))

(define/contract known-parameters
  (listof string?)
  (list "base"
        "timeout"))

(define/contract (make-parameter-identifier-expression n)
  (string? . -> . parameter-identifier-expression?)
  (unless (list? (member n known-parameters string=?))
    (error (format "Unknown parameter \"~a\". (The only acceptable parameters are: ~a)"
                   n
                   known-parameters)))
  (new parameter-identifier-expression%
       [name n]))

(define/contract (make-environment-variable-identifier-expression n #:fallback [f #f])
  (->* (string?)
       (#:fallback (or/c false/c string?))
       environment-variable-identifier-expression?)
  (new environment-variable-identifier-expression%
       [name n]
       [fallback f]))

(define response-code-identifier%
  (class identifier-expression%
    (super-new)
    (inherit-field name)
    (define/override (evaluate env)
      env)
    (define/override (render)
      (format "+~a" name))))

(define/contract (response-code-identifier? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x response-code-identifier%)))

(define (make-response-code-identifier)
  (new response-code-identifier%
       [name "code"]))
