#lang racket/base

(provide #%module-begin
         #%app
         #%top
         #%datum
         riposte-program
         expression
         normal-identifier
         env-identifier
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
         unset
         schema-ref
         jp-existence
         equality)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         json
         json-pointer
         argo
         net/url
         racket/format
         racket/pretty
         racket/port
         racket/class
         racket/match
         (file "cmd.rkt")
         (file "response.rkt")
         (file "json.rkt")
         (file "parameters.rkt")
         (only-in (file "util.rkt")
                  file-content/bytes
                  equal-jsexprs?))

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
         (response-code-matches? response-code))]
    [(_ method:string payload uri (positive-satisfies schema))
     #'(begin
         (displayln (~a "About to inspect"))
         (pretty-print schema)
         (unless (json-schema? schema)
           (error (format "Purported schema is not actually a JSON Schema:~a~a"
                          #\newline
                          (jsexpr->string schema))))
         (cmd/payload method uri payload)
         (unless (adheres-to-schema? (last-response->jsexpr) schema)
           (error "Response does not satisfy schema.")))]
    [(_ method:string payload uri (with-headers headers) (positive-satisfies schema))
     #'(begin
         (unless (json-object? headers)
           (error (format "Headers is not a JSON object! ~a" (pretty-print headers))))
         (for ([(k v) (in-hash headers)])
           (unless (string? v)
             (error (format "Value for property \"~a\" is not a string: ~a" k v))))
         (unless (json-schema? schema)
           (error (format "Purported schema is not actually a JSON Schema:~a~a"
                          #\newline
                          (json-pretty-print schema))))
         (cmd/payload method uri payload #:headers headers)
         (unless (adheres-to-schema? (send last-response as-json) schema)
           (error "Response does not satisfy schema.")))]))

(define-syntax (normal-identifier stx)
  (syntax-parse stx
    [(_ ident:string)
     (format-id #'ident "~a" (syntax->datum #'ident))]))

(define-syntax (env-identifier stx)
  (syntax-parse stx
    [(_ ident:string fallback:string)
     #'(match (getenv ident)
         [(? string? s)
          s]
         [else
          fallback])]
    [(_ ident:string)
     #'(match (getenv ident)
         [(? string? s)
          s]
         [else
          (error (format "Environment variable \"~a\" not set and no fallback was provided." ident))])]))

(define-syntax (normal-assignment stx)
  (syntax-parse stx
    [(_ ident:string expr)
     (with-syntax [(name (format-id #'ident "~a" (syntax->datum #'ident)))]
       #'(define name expr))]
    [(_ ident:string expr "(" type ")")
     (with-syntax [(name (format-id #'ident "~a" (syntax->datum #'ident)))]
       #'(begin
           (define name expr)
           (has-type name "is" type)))]))


(define-syntax (parameter-assignment stx)
  (syntax-parse stx
    [(_ "base" expr)
     #'(begin
         (param-base-url (string->url expr)))]
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
     #'(make-immutable-hasheq (list item ...))]))

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
     #'l]
    [(_ i:id)
     (define s (symbol->string (syntax-e #'i)))
     #`(render #,s)]))

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

(define-syntax (schema-ref stx)
  (syntax-parse stx
    [(_ "in" path:string)
     #'(begin
         (let* ([full-path (expand-path path)]
                [bs (file-content/bytes (expand-path path))])
           (with-handlers ([exn:fail:contract? (lambda (e)
                                                 (error (format "Content of ~a is malformed UTF-8." (path->string full-path))))])
             (let ([s (bytes->string/utf-8 bs)])
               (with-handlers ([exn:fail? (lambda (e)
                                            (error (format "Content of ~a is malformed JSON." (path->string full-path))))])
                 (displayln (format "fuck, looking in ~a" (path->string full-path)))
                 (string->jsexpr s))))))]
    [(_ s)
     #'(begin
         (displayln "looking at a literal")
         s)]))

#;
(define-syntax (import stx)
  (syntax-parse stx
    [(_ path:string)
     #'(dynamic-require (expand-path path) #f)]))

(define-syntax (expand-path stx)
  (syntax-parse stx
    [(_ path:string)
     #'(let ([p (cond [(path? (param-cwd))
                       (build-path (param-cwd) path)]
                      [else
                       (string->path path)])])
         (unless (file-exists? p)
           (error (format "No such file: ~a" (path->string p))))
         p)]))

(define-syntax (jp-existence stx)
  (syntax-parse stx
    [(_ jp:string "exists")
     #'(json-pointer-exists? jp)]))

(define-syntax (equality stx)
  (syntax-parse stx
    [(_ lhs rhs)
     #`(unless (equal-jsexprs? lhs rhs)
         (error (format "Equation fails: ~a is ~a, but ~a is ~a."
                        (render lhs)
                        (json-pretty-print lhs)
                        (render rhs)
                        (json-pretty-print rhs))))]))
