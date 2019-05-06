#lang racket/base

(provide make-response
         response?)

(require racket/class
         racket/contract
         ejs
         racket/function
         (only-in (file "util.rkt")
                  bytes->string))

(define response%
  (class object%
    (super-new)
    (init-field code
                headers
                body/raw)
    (define/public (has-body?)
      (bytes=? body/raw #""))
    (define/public (as-ejsexpr)
      (bytes->ejsexpr body/raw))
    (define/public (body-is-well-formed?)
      (cond [(bytes=? #"" body/raw)
             #f]
            [else
             (with-handlers ([exn:fail? (const #f)])
               (begin0
                   #t
                 (send this as-ejsexpr)))]))
    (define/public (get-code)
      code)
    (define/public (body-is-string?)
      (string? (bytes->string body/raw)))
    (define/public (body-bytes-length)
      (bytes-length body/raw))))

(define (response? x)
  (and (object? x)
       (is-a? x response%)))

(define/contract (make-response code headers body)
  ((integer-in 100 599) (and/c immutable? (hash/c symbol? string?)) bytes? . -> . response?)
  (new response%
       [code code]
       [headers headers]
       [body/raw body]))
