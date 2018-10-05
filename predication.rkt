#lang racket/base

(provide make-header-presence-predication
         make-header-absence-predication
         header-predication?
         make-json-pointer-exists-predication
         predication?)

(require racket/class
         racket/contract
         (only-in json-pointer
                  json-pointer-value)
         (only-in (file "assertion.rkt")
                  assertion%)
         (only-in (file "identifier.rkt")
                  make-header-identifier-expression)
         (only-in (file "environment.rkt")
                  environment-response
                  environment-response-headers)
         (only-in (file "util.rkt")
                  ejsexpr->jsexpr))

(define predication%
  (class assertion%
    (super-new)))

(define (predication? x)
  (and (object? x)
       (is-a? x predication%)))

(define header-predication%
  (class predication%
    (super-new)
    (init-field name
                present?)
    (define/override (evaluate env)
      (define hs (environment-response-headers env))
      (define ok?
        (if present?
            (hash-has-key? hs name)
            (not (hash-has-key? hs name))))
      (unless ok?
        (if present?
            (error (format "Response header ~a is supposed to be present, but it isn't. Headers are: ~a"
                           name
                           hs))
            (error (format "Response header ~a is supposed to be absent, but it is present (it has the value ~a)"
                           name
                           (hash-ref hs name)))))
      env)
    (define/override (render)
      (define id (make-header-identifier-expression name))
      (format "~a is ~a"
              (send id render)
              (cond [present?
                     "present"]
                    [else
                     "absent"])))))

(define (header-predication? x)
  (and (object? x)
       (is-a? x header-predication%)))

(define/contract (make-header-presence-predication ident)
  (string? . -> . header-predication?)
  (new header-predication%
       [name (string->symbol ident)]
       [present? #t]))

(define/contract (make-header-absence-predication ident)
  (string? . -> . header-predication?)
  (new header-predication%
       [name (string->symbol ident)]
       [present? #f]))

(define json-pointer-exists-predication%
  (class predication%
    (super-new)
    (init-field jp-expr)
    (define/override (evaluate env)
      (define env/jsexpr (ejsexpr->jsexpr (environment-response env)))
      (with-handlers ([exn:fail? (lambda (exn)
                                   (error (format "JSON Pointer ~a does not refer!" jp-expr)))])
        (json-pointer-value jp-expr
                            env/jsexpr))
      env)
    (define/override (render)
      (format "~a exists" jp-expr))))

(define/contract (json-pointer-exists-predication? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x json-pointer-exists-predication%)))

(define/contract (make-json-pointer-exists-predication jp)
  (string? . -> . json-pointer-exists-predication?)
  (new json-pointer-exists-predication%
       [jp-expr jp]))
