#lang racket/base

(provide #%module-begin
         #%app
         #%top
         #%datum
         riposte-program
         expression
         normal-identifier
         command
         normal-assignment
         parameter-assignment
         header-assignment
         uri-template
         uri-template-expression
         uri-template-variable-list
         uri-template-varspec
         json-object
         json-object-item
         json-array
         json-array-item
         json-boolean
         has-type
         json-pointer
         unset)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         json-pointer
         net/url
         racket/format
         racket/pretty
         (file "cmd.rkt")
         (file "response.rkt")
         (file "json.rkt"))

(define-syntax (riposte-program stx)
  (syntax-parse stx
    [(_ step ...)
     #'(begin
        step ...)]))

(define-syntax (expression stx)
  (syntax-parse stx
    [(_ e)
     #'e]))

(define-syntax (command stx)
  (syntax-parse stx
    [(_ method:string uri)
     #'(cmd method uri)]
    [(_ method:string uri (responds-with response-code:string))
     #'(begin
         (cmd method uri)
         (response-code-matches? response-code))]
    [(_ method:string payload uri (responds-with response-code:string))
     #'(begin
         (cmd/payload method uri payload)
         (response-code-matches? response-code))]
    [(_ method:string payload uri (responds-with response-code:string))
     #'(begin
         (cmd/payload method uri payload)
         (response-code-matches? response-code))]
    [(_ method:string payload uri (with-headers headers) (responds-with response-code:string))
     #'(begin
         (unless (json-object? headers)
           (error (format "Headers is not a JSON object! ~a" (pretty-print headers))))
         (for ([(k v) (in-hash headers)])
           (unless (string? v)
             (error (format "Value for property \"~a\" is not a string: ~a" k v))))
         (cmd/payload method uri payload #:headers headers)
         (response-code-matches? response-code))]))

(define-syntax (normal-identifier stx)
  (syntax-parse stx
    [(_ ident:string)
     (format-id #'ident "~a" (syntax->datum #'ident))]))

(define-syntax (normal-assignment stx)
  (syntax-parse stx
    [(_ ident:string expr)
     (with-syntax [(name (format-id #'ident "~a" (syntax->datum #'ident)))]
       #'(define name expr))]))

(define-syntax (parameter-assignment stx)
  (syntax-parse stx
    [(_ "base" expr)
     #'(param-base-url (string->url expr))]
    [(_ "timeout" (expression t:integer))
     #'(begin
         (unless (> t 0)
           (error (format "Timeout should be positive; ~a supplied." t)))
         (param-timeout t))]))

(define-syntax (header-assignment stx)
  (syntax-parse stx
    [(_ ident:string expr)
     #'(hash-set! request-headers (string->symbol ident) expr)]))

(define-syntax (uri-template stx)
  (syntax-parse stx
    [(_ part ...)
     #'(string-append part ...)]))

(define-syntax (uri-template-expression stx)
  (syntax-parse stx
    [(_ e)
     #'e]))

(define-syntax (uri-template-variable-list stx)
  (syntax-parse stx
    [(_ e ...)
     #'(apply string-append (map ~a (list e ...)))]))

(define-syntax (uri-template-varspec stx)
  (syntax-parse stx
    [(_ ident:string)
     (format-id stx "~a" (syntax->datum #'ident))]))

(define-syntax (json-object stx)
  (syntax-parse stx
    [(_ item ...)
     #'(make-hash (list item ...))]))

(define-syntax (json-object-item stx)
  (syntax-parse stx
    [(_ prop:string expr)
     #'(cons (string->symbol prop) expr)]))

(define-syntax (json-array stx)
  (syntax-parse stx
    [(_ item ...)
     #'(list item ...)]))

(define-syntax (json-array-item stx)
  (syntax-parse stx
    [(_ i)
     #'i]))

(define-syntax (has-type stx)
  (syntax-parse stx
    [(_ expr "is" type)
     (syntax-parse #'type
       [(json-type (json-number-type (arithmetical-adjective "positive") "integer"))
        #'(begin
            (unless (integer? expr)
              (error (format "~a is not an integer!" (render expr))))
            (unless (> expr 0)
              (error (format "~a is not positive!" (render expr)))))])]))

(define-syntax (json-pointer stx)
  (syntax-parse stx
    [(_ jp:string)
     #'(json-pointer-value jp (last-response->jsexpr))]))

(define-syntax (render stx)
  (syntax-parse stx
    [(_ (expression e))
     #'(render e)]
    [(_ l:string)
     #'l]))

(define-syntax (unset stx)
  (syntax-parse stx
    [(_ header:string)
     #'(hash-remove! request-headers (string->symbol header))]))

(define-syntax (json-boolean stx)
  (syntax-parse stx
    [(_ "true")
     #'#t]
    [(_ "false")
     #'#f]))
