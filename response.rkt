#lang racket/base

(provide make-response
         response?)

(require racket/class
         racket/contract
         racket/function
         json
         (only-in (file "util.rkt")
                  bytes->string))

(define response%
  (class object%
    (super-new)
    (init-field code
                headers
                body/raw)
    (define/public (has-body?)
      (not (bytes=? body/raw #"")))
    (define/public (as-jsexpr)
      (bytes->jsexpr body/raw))
    (define/public (body-is-well-formed?)
      (cond [(bytes=? #"" body/raw)
             #f]
            [else
             (with-handlers ([exn:fail? (const #f)])
               (begin0
                   #t
                 (send this as-jsexpr)))]))
    (define/public (get-code)
      code)
    (define/public (get-headers)
      headers)
    (define/public (body-is-string?)
      (string? (bytes->string body/raw)))
    (define/public (body-bytes-length)
      (bytes-length body/raw))
    (define/public (get-body/raw)
      body/raw)))

(define (response? x)
  (and (object? x)
       (is-a? x response%)))

(define/contract (make-response code headers body)
  ((integer-in 100 599) (and/c immutable? (hash/c symbol? string?)) bytes? . -> . response?)
  (define (lowercase k v)
    (cons (string->symbol
           (string-downcase
            (symbol->string k)))
          v))
  (define headers/lowercased
    (make-immutable-hash (hash-map headers lowercase)))
  (new response%
       [code code]
       [headers headers/lowercased]
       [body/raw body]))
