#lang racket/base

(provide make-response-emptiness-expression
         make-response-nonemptiness-expression
         empty-response-step?
         make-json-pointer-empty-expression
         make-json-pointer-nonempty-expression)

(require racket/contract
         racket/class
         racket/match
         (only-in racket/format
                  ~a)
         (only-in racket/port
                  with-output-to-string)
         (only-in racket/list
                  empty?)
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string)
         (only-in racket/string
                  string-split)
         (only-in json-pointer
                  json-pointer-value
                  json-pointer-refers?)
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

(define/contract empty-response%
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
        [#t
         "is empty"]
        [#f
         "is non empty"]))))

(define/contract empty-json-pointer%
  class?
  (class assertion%
    (super-new)
    (init-field sense expr base)
    (define/override (evaluate env)
      (define doc
        (cond [(identifier-expression? base)
               (send base evaluate env)]
              [else
               (environment-response env)]))
      (unless (or (hash? doc)
                  (list? doc))
        (error
         (cond [(identifier-expression? base)
                (format "~a is neither an array nor an object; cannot evaluate JSON Pointer expression \"~a\"."
                        (send base render)
                        expr)]
               [else
                (format "The previous response is neither an array nor an object; cannot evaluate JSON Pointer expression \"~a\"."
                        expr)])))
      (unless (json-pointer-refers? expr doc)
        (define new-message
          (with-output-to-string
            (lambda ()
              (displayln (format "JSON Pointer \"~a\" does not refer!" expr))
              (displayln (format "We evaluated the JSON Pointer relative to:"))
              (displayln (ejsexpr->string doc)))))
        (error new-message))
      (define val (json-pointer-value expr doc))
      (cond [(string? val)
             (match sense
               [#t
                (unless (string=? "" val)
                  (error (format "~a evaluates to a non-empty string (\"~a\")." expr val)))]
               [#f
                (when (string=? "" val)
                  (error (format "~a evaluates to the empty string." expr val)))])]
            [(list? val)
             (match sense
               [#t
                (unless (empty? val)
                  (error (format "~a evaluates to a non-empty array."expr)))]
               [#f
                (when (empty? val)
                  (error (format "~a evaluates to the empty array." expr)))])]
            [(hash? val)
             (match sense
               [#t
                (unless (hash-empty? val)
                  (error (format "~a evaluates to a non-empty object."expr)))]
               [#f
                (when (hash-empty? val)
                  (error (format "~a evaluates to the empty object." expr)))])]
            [else
             (error (format "\"~a\" refers to a value that is neither a string, nor an array, nor an object. Cannot check emptyiness." expr))])
      env)
    (define/override (render)
      (match sense
        [#f
         "is empty"]
        [#t
         "is non empty"]))))

(define/contract (empty-response-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x empty-response%)))

(define/contract (empty-json-pointer-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x empty-json-pointer%)))

(define/contract (make-response-emptiness-expression)
  (-> empty-response-step?)
  (new empty-response%
       [sense #t]))

(define/contract (make-response-nonemptiness-expression)
  (-> empty-response-step?)
  (new empty-response%
       [sense #f]))

(define/contract (make-json-pointer-empty-expression expr base)
  (string? (or/c false/c identifier-expression?) . -> . empty-json-pointer-step?)
  (new empty-json-pointer%
       [sense #t]
       [base base]
       [expr expr]))

(define/contract (make-json-pointer-nonempty-expression expr base)
  (string? (or/c false/c identifier-expression?) . -> . empty-json-pointer-step?)
  (new empty-json-pointer%
       [sense #f]
       [base base]
       [expr expr]))
