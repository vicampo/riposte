#lang br/quicklang

(require racket/contract
         http
         ejs
         argo
         net/url
         json-pointer
         racket/syntax
         racket/include
         racket/dict
         racket/hash
         (file "evaluator.rkt")
         (only-in (file "util.rkt")
                  bytes->string)
         (only-in (file "command.rkt")
                  make-command-expression)
         (only-in (file "json-pointer.rkt")
                  make-json-pointer-expression)
         (only-in (file "identifier.rkt")
                  make-variable-identifier-expression
                  make-parameter-identifier-expression
                  make-header-identifier-expression
                  make-environment-variable-identifier-expression)
         (only-in (file "json.rkt")
                  make-json-object-expression)
         (only-in (file "assertion.rkt")
                  make-response-code-matches-expression)
         (only-in (file "import.rkt")
                  make-import)
         (only-in (file "parameters.rkt")
                  param-environment)
         (only-in (file "step.rkt")
                  step?)
         (only-in (file "./version.rkt")
                  riposte-version)
         (only-in (file "grammar.rkt")
                  parse)
         (only-in (file "response.rkt")
                  response?
                  make-response)
         (file "setup.rkt"))

(require (for-syntax (only-in (file "grammar.rkt")
                              parse)
                     (prefix-in reader:
                                (only-in (file "reader.rkt")
                                         read-syntax))
                     (only-in (file "new-tokenizer.rkt")
                              tokenize)
                     (only-in (file "parameters.rkt")
                              param-cwd)
                     racket/syntax
                     syntax/stx
                     racket/include))

(define/contract
  base-url
  (or/c false/c url?)
  #f)

(define/contract (update-base-url! new-base-url)
  (string? . -> . void)
  (set! base-url (string->url new-base-url)))

(define/contract
  default-request-headers
  (and/c immutable? (hash/c symbol? string?))
  (hasheq 'User-Agent (format "Riposte/~a (https://riposte.in)" riposte-version)))

(define/contract
  request-headers
  (and/c (hash/c symbol? string?)
         (not/c immutable?))
  (make-hasheq (hash->list default-request-headers)))

(define/contract
  last-response-code
  (or/c false/c (integer-in 100 599))
  #f)

(define/contract
  last-response-body
  (or/c false/c bytes?)
  #f)

(define/contract
  last-response-ejsexpr
  (or/c void ejsexpr?)
  void)

(define/contract (read-entity/bytes+response-code in h)
  (input-port? string? . -> . (list/c exact-integer? dict? bytes?))
  (list (extract-http-code h)
        (heads-string->dict h)
        (read-entity/bytes in h)))

(define/contract (request method url headers)
  (string?
   string?
   (or/c false/c
         (hash/c symbol? string?))
   . -> .
   (list/c exact-integer? (and/c immutable? (hash/c symbol? string?)) bytes?))
  (define (network-fail e)
    (error (format "Failed to connect to ~a!" url)))
  (define (died e)
    (log-error "~a" (exn-message e))
    (error (format "Something weird happened when sending a ~a request to ~a!"
                   method
                   url)))
  (with-handlers ([exn:fail:network? network-fail]
                  [exn? died])
    (call/input-request "1.1"
                        method
                        url
                        headers
                        read-entity/bytes+response-code)))

(define/contract (request/payload method url headers payload)
  (string?
   string?
   (or/c false/c
         (hash/c symbol? string?))
   ejsexpr?
   . -> .
   (list/c exact-integer? (and/c immutable? (hash/c symbol? string?)) bytes?))
  (log-error "~a ~a with payload ~a" method url payload)
  (define (network-fail e)
    (error (format "Failed to connect to ~a!" url)))
  (define (died e)
    (log-error "~a" (exn-message e))
    (error (format "Something weird happened when sending a ~a request to ~a!"
                   method
                   url)))
  (with-handlers ([exn:fail:network? network-fail]
                  [exn? died])
    (call/output-request "1.1"
                         method
                         url
                         (ejsexpr->bytes payload)
                         #f
                         headers
                         read-entity/bytes+response-code)))

(define/contract
  last-response
  (or/c false/c response?)
  #f)

(define/contract (update-last-response! code headers body)
  ((integer-in 100 599) (and/c immutable? (hash/c symbol? string?)) bytes? . -> . void)
  (set! last-response
        (make-response code headers body)))

(define/contract (cmd method url)
  (string? string? . -> . void)
  (define final-url
    (cond [(url? base-url)
           (url->string (combine-url/relative base-url url))]
          [else
           url]))
  (display (format "~a ~a" method final-url))
  (flush-output)
  (define result (request method final-url request-headers))
  (match result
    [(list code headers body)
     (displayln (format " responds with ~a" code))
     (update-last-response! code headers body)]))

(define/contract (cmd/payload method url payload)
  (string? string? ejsexpr? . -> . void)
  (displayln (format "headers = ~a" request-headers))
  (define final-url
    (cond [(url? base-url)
           (url->string (combine-url/relative base-url url))]
          [else
           url]))
  (display (format "~a ~a" method final-url))
  (flush-output)
  (define result (request/payload method final-url request-headers payload))
  (match result
    [(list code headers body)
     (displayln (format " responds with ~a" code))
     (update-last-response! code headers body)]))

(define/contract (response-code-matches-pattern? received-code expected-code)
  (string? string? . -> . boolean?)
  (cond [(string=? "" received-code)
         (string=? "" expected-code)]
        [(string=? "" expected-code)
         #f]
        [else
         (define c1 (string-ref received-code 0))
         (define c2 (string-ref expected-code 0))
         (cond [(or (char=? c2 #\X)
                    (char=? c2 #\x))
                (response-code-matches-pattern? (substring received-code 1)
                                                (substring expected-code 1))]
               [(char=? c1 c2)
                (response-code-matches-pattern? (substring received-code 1)
                                                (substring expected-code 1))]
               [else
                #f])]))

(define/contract (response-code-matches? code)
  (string? . -> . void)
  (define r last-response)
  (unless (response? r)
    (error "No response has been received yet, so we cannot check whether response code matches \"~a\"."
           code))
  (define matches?
    (response-code-matches-pattern? (format "~a" (send r get-code))
                                    code))
  (unless matches?
    (error
     (format "The received response has code ~a, but we expected ~a."
             (send r get-code)
             code))))

(define/contract (response-satisfies-schema? schema)
  (ejsexpr? . -> . void)
  (define r last-response)
  (unless (response? r)
    (error "No response has been received yet, so we cannot check whether response code adheres to a schema."))
  (unless (send last-response body-is-well-formed?)
    (error "The previous response is malformed JSON."))
  (unless (json-schema? schema)
    (define error-message
      (with-output-to-string
        (lambda ()
          (displayln "The given JSON datum is not a JSON Schema:")
          (displayln (ejsexpr->string schema)))))
    (error error-message))
  (unless (adheres-to-schema? (send last-response as-ejsexpr) schema)
    (define error-message
      (with-output-to-string
        (lambda ()
          (displayln "The given JSON datum does not adhere to the JSON Schema."))))
    (error error-message)))

(define/contract (check-response-empty)
  (-> void)
  (define r last-response)
  (unless (response? r)
    (error "No response has been received yet, so we cannot check whether it is empty."))
  (unless (send r has-body?)
    (error
     (format "The previous response is not empty; ~a bytes were received."
             (send r body-bytes-length)))))

(define/contract (check-response-nonempty)
  (-> void)
  (define r last-response)
  (unless (response? r)
    (error "No response has been received yet, so we cannot check whether it is empty."))
  (when (send r has-body?)
    (error "The previous response is empty.")))

(define/contract (check-equal lhs rhs)
  (ejsexpr? ejsexpr? . -> . void)
  (unless (equal-ejsexprs? lhs rhs)
    (error (format "JSON values are not equal! LHS: ~a RHS: ~a" lhs rhs))))

(define-macro (riposte-module-begin PARSE-TREE)
  #`(#%module-begin
     (module configure-runtime br
       (require riposte/setup)
       (do-setup!))
     #'PARSE-TREE
     #;(unsyntax-splicing #'PARSE-TREE)
     ))

(provide (rename-out [riposte-module-begin #%module-begin]))

(define-macro (riposte-program STEPS ...)
  #'(begin STEPS ...))

(provide riposte-program)

(define-macro (program-step STEP)
  #'STEP)

(provide program-step)

(define-macro (assignment ASS)
  #'ASS)

(provide assignment)

(define-macro (normal-assignment ID EXPR)
  (with-syntax ([name (format-id #'ID "~a" (syntax->datum #'ID))])
    #'(define name EXPR)))

(provide normal-assignment)

(define-macro-cases parameter-assignment
  [(parameter-assignment "base" URL)
   #'(update-base-url! URL)])

(provide parameter-assignment)

(define-macro (header-assignment ID EXPR)
  #'(hash-set! request-headers (string->symbol ID) EXPR))

(provide header-assignment)

(define/contract (check-json-pointer-refers jp)
  (string? . -> . void)
  (log-error "checking whether ~a refers" jp)
  (define r last-response)
  (unless (response? r)
    (error (format "No response has been received yet, so we cannot check whether \"~a\" refers."
                   jp)))
  (unless (send r body-is-string?)
    (error "Response body is not a well-formed sequence of UTF-8 bytes."))
  (unless (send r body-is-well-formed?)
    (error "Response body is malformed as JSON."))
  (unless (json-pointer-refers? jp (send r as-ejsexpr))
    (error (format "JSON Pointer \"~a\" does not refer." jp))))

(define/contract (check-json-pointer-refers-to-nonempty-value jp)
  (string? . -> . void)
  (define r last-response)
  (unless (response? r)
    (error (format "No response has been received yet, so we cannot check whether \"~a\" refers."
                   jp)))
  (unless (send r body-is-string?)
    (error "Response body is not a well-formed sequence of UTF-8 bytes."))
  (unless (send r body-is-well-formed?)
    (error "Response body, viewed as JSON, is malformed:"))
  (define body/ejsexpr (send r as-ejsexpr))
  (unless (json-pointer-refers? jp body/ejsexpr)
    (error (format "JSON Pointer \"~a\" does not refer." jp)))
  (define v (json-pointer-value jp body/ejsexpr))
  (unless (or (hash? v)
              (string? v)
              (list? v))
    (error "Emptiness is defined only for objects, strings and lists."))
  (cond [(hash? v)
         (when (hash-empty? v)
           (error (format "\"~a\" refers to a non-empty hash!" jp)))]
        [(string? v)
         (when (string=? "" v)
           (error (format "\"~a\" refers to a non-empty string!" jp)))]
        [(list? v)
         (when (empty? v)
           (error (format "\"~a\" refers to a non-empty list!" jp)))]))

(define-macro-cases predication
  [(predication (json-pointer JP) "exists")
   #'(check-json-pointer-refers (json-pointer-as-string JP))]
  [(predication (json-pointer JP) "exists" "and" "is" "non" "empty")
   #'(check-json-pointer-refers-to-nonempty-value (json-pointer-as-string JP))])

(provide predication)

(define-macro-cases jp-existence
  [(_ JP "exists")
   #'(begin
      (unless (response? last-response)
        (error "No response received yet; JSON Pointer \"~a\" cannot refer to anything." jp))
      (unless (send last-response body-is-string?)
        (error "Body cannot be converted to a string (is it binary data?)"))
      (unless (send last-response body-is-well-formed?)
        (error "Body is malformed JSON."))
      (unless (json-pointer-refers? JP (send last-response as-ejsexpr))
        (define new-message
          (with-output-to-string
            (lambda ()
              (displayln (format "JSON Pointer \"~a\" does not refer!" JP))
              (displayln (format "We evaluated the JSON Pointer relative to:"))
              (displayln (ejsexpr->string (send last-response as-ejsexpr))))))
        (error new-message)))])

(provide jp-existence)

(define-syntax (expression stx)
  (syntax-case stx ()
    [(_ (json-expression jp))
     #'jp]
    [(_ x "+" y)
     #'(cond [(hash? x)
              (unless (hash? y)
                (error "Cannot add an object and a non-object."))
              (hash-union x y)]
             [(real? x)
              (unless (real? y)
                (error "Cannot add a number and a non-number."))
              (+ x y)]
             [(string? x)
              (unless (string? y)
                (error "Cannot add a string and a non-string."))
              (string-append x y)]
             [else
              (error "How do add ~a and ~a?" x y)])]
    ; not sure what to do; punt
    [(_ x)
     #'x]))

(provide expression)

(define-macro (json-expression JSEXPR)
  #'JSEXPR)

(provide json-expression)

(define-macro (json-number NUMBER)
  #'NUMBER)

(provide json-number)

(define-macro (json-integer DIGITS ...)
  #'(string->number (foldr (lambda (a b) (format "~a~a" a b))
                           ""
                           (list DIGITS ...))))

(provide json-integer)

(define-macro (json-float DIGITS ...)
  #'(parameterize ([read-decimal-as-inexact #f])
      (string->number (foldr (lambda (a b) (format "~a~a" a b))
                             ""
                             (list DIGITS ...)))))

(provide json-float)

#;
(define-macro (json-boolean B)
  #'B)

(define-macro-cases json-boolean
  [(json-boolean "true")
   #'#t]
  [(json-boolean #t)
   #'#t]
  [(json-boolean "false")
   #'#f]
  [(json-boolean #f)
   #'#f])

(provide json-boolean)

(define-macro (json-object ITEMS ...)
  #'(make-immutable-hasheq (list ITEMS ...)))

(provide json-object)

(define-macro (json-object-item PROP EXPR-OR-ID)
  #'(cons (string->symbol PROP) EXPR-OR-ID))

(provide json-object-item)

(define-macro (json-object-property PROP)
  #'PROP)

(provide json-object-property)

(define-macro (json-array "[" ITEMS ... "]")
  #'(list ITEMS ...))

(provide json-array)

(define-macro (json-array-item ITEM)
  #'ITEM)

(provide json-array-item)

(define-macro (json-string S)
  #'S)

(provide json-string)

(define-macro (json-null S)
  #''null)

(provide json-null)

(define/contract (fetch-environment-variable var fallback)
  (string-environment-variable-name? (or/c false/c string?) . -> . (or/c false/c string?))
  (define val (getenv var))
  (cond [(string? val)
         val]
        [else
         fallback]))

(define-syntax (id stx)
  (syntax-case stx ()
    [(_ (normal-identifier x))
     (format-id stx "~a" (syntax->datum #'x))]
    [(_ (env-identifier x))
     #'(begin
         (when (eq? #f (getenv x))
           (error "Undefined environment variable (~a)." x))
         (getenv x))]
    [(_ (env-identifier x "with" "fallback" f))
     #'(fetch-environment-variable x f)]))

(provide id)

(define-syntax (env-identifier stx)
  (syntax-case stx ()
    [(_ x)
     #'(begin
         (when (eq? #f (getenv x))
           (error (format "Undefined environment variable (~a)." x)))
         (getenv x))]
    [(_ x f)
     #'(fetch-environment-variable x f)]))

(provide env-identifier)

(define-macro (parameter-identifier PARAM)
  #'(make-parameter-identifier-expression PARAM))

(provide parameter-identifier)

(define-syntax (normal-identifier stx)
  (syntax-case stx ()
    [(_ id)
     (format-id stx "~a" (syntax->datum #'id))]))

(provide normal-identifier)

(define-macro (head-id ID)
  #'ID)

(provide head-id)

(define-macro (http-method METHOD-CHARS ...)
  #'(~a METHOD-CHARS ...))

(provide http-method)

(define-macro (uri-template EXPRS ...)
  #'(~a EXPRS ...))

(provide uri-template)

(define-macro (uri-template-literals LITERALS ...)
  #'(~a LITERALS ...))

(provide uri-template-literals)

(define-macro (digit D)
  #'D)

(provide digit)

(define-macro-cases uri-template-expression
  [(_ (uri-template-variable-list VAR))
   #'VAR])

(provide uri-template-expression)

(define-macro-cases uri-template-varspec
  [(_ VAR)
   (format-id #'VAR "~a" (syntax->datum #'VAR))])

(provide uri-template-varspec)

(provide)

(define-macro (uri-template-varname PIECES ...)
  #'(string-append PIECES ...))

(provide uri-template-varname)

(define-macro-cases command
  [(_ METHOD URI)
   #'(cmd METHOD URI)]
  [(_ METHOD URI (responds-with CODE))
   #'(begin
       (cmd METHOD URI)
       (response-code-matches? CODE))]
  [(_ METHOD URI (responds-with CODE) "and" (emptiness "is" "non" "empty"))
   #'(begin
       (cmd METHOD URI)
       (response-code-matches? CODE)
       (check-response-nonempty))]
  [(_ METHOD PAYLOAD URI (responds-with CODE))
   #'(begin
       (cmd/payload METHOD URI PAYLOAD)
       (response-code-matches? CODE))]
  [(_ METHOD PAYLOAD URI (responds-with CODE) "and" (emptiness "is" "non" "empty"))
   #'(begin
       (cmd/payload METHOD URI PAYLOAD)
       (response-code-matches? CODE)
       (check-response-nonempty))]
  [(_ METHOD URI (responds-with CODE) (positive-satisfies SCHEMA))
   #'(begin
       (cmd METHOD URI)
       (response-code-matches? CODE)
       (response-satisfies-schema? SCHEMA))]
  [(_ METHOD URI (positive-satisfies (schema-ref SCHEMA)))
   #'(begin
       (cmd METHOD URI)
       (response-satisfies-schema? SCHEMA))])

(provide command)

(define-macro (assertion ASS)
  #'ASS)

(provide assertion)

(define-macro (equality LHS RHS)
  #'(check-equal LHS RHS))

(provide equality)

(define-macro-cases http-response-code
  [(http-response-code DIGIT-1 DIGIT-2 DIGIT-3)
   #'(format "~a~a~a"
             DIGIT-1
             DIGIT-2
             DIGIT-3)]
  [(http-response-code CODE)
   #'(format "~a" CODE)])

(provide http-response-code)

(define-macro (json-pointer JP)
  #'(begin
      (unless (response? last-response)
        (error "No response received yet; JSON Pointer \"~a\" cannot refer to anything." jp))
      (unless (send last-response body-is-string?)
        (error "Body cannot be converted to a string (is it binary data?)"))
      (unless (send last-response body-is-well-formed?)
        (error "Body is malformed JSON."))
      (json-pointer-value JP (send last-response as-ejsexpr))))

(provide json-pointer)

(define-macro-cases json-pointer-as-string
  [(json-pointer-as-string (bare-json-pointer PARTS ...))
   #'(string-append PARTS ...)])

(provide json-pointer-as-string)

(define-macro (bare-json-pointer PARTS ...)
  #'(let [(jp (string-append PARTS ...))]
      (unless (response? last-response)
        (error "No response received yet; JSON Pointer \"~a\" cannot refer to anything." jp))
      (unless (send last-response body-is-string?)
        (error "Body cannot be converted to a string (is it binary data?)"))
      (unless (send last-response body-is-well-formed?)
        (error "Body is malformed JSON."))
      (json-pointer-value jp (send last-response as-ejsexpr))))

(provide bare-json-pointer)

(define-macro (relative-json-pointer (bare-json-pointer PARTS ...) "relative to" ID)
  #'(json-pointer-value (string-append PARTS ...)
                        ID))

(provide relative-json-pointer)

(define-macro (reference-token BITS ...)
  #'(string-append BITS ...))

(provide reference-token)

(define-macro (unescaped-token BITS ...)
  #'(string-append BITS ...))

(provide unescaped-token)

(define-macro (letter L)
  #'L)

(provide letter)

(define-macro (riposte-repl THING)
  #'THING)

(provide riposte-repl)
