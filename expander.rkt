#lang racket/base

(provide #%module-begin
         #%app
         #%top
         #%datum
         #%top-interaction
         riposte-program
         expression
         program-step
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
         equality
         disequality
         inequality
         header-presence
         response-head-id
         sequence-predicate
         echo
         exec
         exec-arg-item
         riposte-repl
         bang)

(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     syntax/stx
                     racket/syntax)
         json
         json-pointer
         argo
         net/url
         racket/format
         racket/function
         racket/pretty
         racket/port
         racket/system
         racket/class
         racket/match
         racket/string
         racket/hash
         racket/list
         br/define
         misc1/syntax
         (file "cmd.rkt")
         (file "response.rkt")
         (file "json.rkt")
         (file "parameters.rkt")
         (only-in (file "util.rkt")
                  file-content/bytes
                  hash-remove*
                  comment-out-lines))

(define param-timeout
  (make-parameter 30))

(define-syntax (riposte-program stx)
  (syntax-parse stx
    [(_ step ...)
     #`(begin
         (check-environment-variables (riposte-program step ...))
         step ...)]))

(define-syntax (expression stx)
  (syntax-parse stx
    [(_ e)
     #'e]
    [(_ e1 "+" e2)
     #'(cond [(string? e1)
              (unless (string? e2)
                (error (format "~a works out to a string, but ~a is not a string. Addition is not defined in this case." (render e1) (render e2))))
              (string-append e1 e2)]
             [(hash? e1)
              (unless (hash? e2)
                (error (format "~a works out to be an object, but ~a is not an object. Addition is not defined in this case." (render e1) (render e2))))
              (hash-union e1 e2)]
             [(number? e1)
              (unless (number? e2)
                (error (format "~a works out to be a number, but ~a is not a number. Addition is not defined in this case." (render e1) (render e2))))
              (+ e1 e2)]
             [(list? e1)
              (unless (list? e2)
                (error (format "~a works out to be an array, but ~a is not an array. Addition is not defined in this case." (render e1) (render e2))))
              (append e1 e2)]
             [(eq? 'null e1)
              (error (format "~a is null; addition is not defined." (render e1)))]
             [(boolean? e1)
              (error (format "~a is a boolean; addition is not defined."))])]
    [(_ e1 "-" e2)
     #'(cond [(string? e1)
              (unless (string? e2)
                (error (format "~a works out to a string, but ~a is not a string. Subtraction is not defined in this case." (render e1) (render e2))))
              (string-replace e1 e2 "")]
             [(hash? e1)
              (unless (hash? e2)
                (error (format "~a works out to be an object, but ~a is not an object. Subtraction is not defined in this case." (render e1) (render e2))))
              (hash-remove* e1 (hash-keys e2))]
             [(number? e1)
              (unless (number? e2)
                (error (format "~a works out to be a number, but ~a is not a number. Subtraction is not defined in this case." (render e1) (render e2))))
              (- e1 e2)]
             [(list? e1)
              (unless (list? e2)
                (error (format "~a works out to be an array, but ~a is not an array. Subtraction is not defined in this case." (render e1) (render e2))))
              (remove* e1 e2 equal-jsexprs?)]
             [(eq? 'null e1)
              (error (format "~a is null; subtraction is not defined." (render e1)))]
             [(boolean? e1)
              (error (format "~a is a boolean; subtraction is not defined."))])]
    [(_ e1 "*" e2)
     #'(cond [(string? e1)
              (cond [(integer? e2)
                     (when (< e2 0)
                       (error (format "~a works out to a string, but ~a is a negative integer. Multiplication is not defined in this case." (render e1) (render e2))))
                     (apply string-append
                            (for/list ([n (in-range 1 e2)])
                              e1))]
                    [(number? e2)
                     (error (format "~a works out to be a s tring, but ~a is not a positive integer. Multiplication is not defined in this case." (render e1) (render e2)))]
                    [else
                     (error (format "~a works out to a string, but ~a is not an integer. Multiplication is not defined in this case." (render e1) (render e2)))])]
             [(hash? e1)
              (error (format "~a works out to be an object; multiplication is not defined in this case." (render e1)))]
             [(integer? e1)
              (cond [(string? e2)
                     (unless (< e1 0)
                       (error (format "~a works out to be a negative integer; multiplication not defined in this case." (render e1))))
                     (apply string-append
                            (for/list ([n (in-range 1 e1)])
                              e2))]
                    [(number? e2)
                     (* e1 e2)]
                    [else
                     (error (format "~a works out to be a positive integer, but ~a is not a string. Multiplication not defined in this case." (render e1) (render e2)))])]
             [(number? e1)
              (unless (number? e2)
                (error (format "~a works out to be a non-integer number, but ~a is not a number. Multiplication not defined in this case." (render e1) (render e2))))
              (* e1 e2)]
             [(list? e1)
              (error (format "~a works out to be an array; multiplciation is not defined in this case." (render e1)))]
             [(eq? 'null e1)
              (error (format "~a is null; multiplication is not defined." (render e1)))]
             [(boolean? e1)
              (error (format "~a is a boolean; multiplication is not defined."))])]))

(define-macro-cases render-items
  [(_ (json-object-item K V) (json-object-item K1 V1) ...)
   #'(string-append (render K)
                    ":"
                    (render V))])

(define-macro-cases render
  [(_ (expression E))
   #'(render E)]
  [(_ (normal-identifier ID))
   #'(~a #\$ (syntax-e #'ID))]
  [(_ (response-head-id ID))
   #'(~a (syntax-e #'ID) #\^)]
  [(_ (json-object ITEMS ...))
   #'(string-append "{"
                    (render-items ITEMS ...)
                    "}")]
  [(_ (json-pointer JP))
   #'JP]
  [(_ (bang B))
   #'(string->immutable-string (make-string B #\!))]
  [else
   (syntax-parse caller-stx
     [(_ l:string)
      #'l]
     [(_ n:number)
      #'n]
     [(_ i:id)
      #'(~a #\$ (symbol->string (syntax-e #'i)))])])

(define-macro-cases command
  [(_ METHOD URI)
   #'(begin
       (cmd METHOD URI #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1)))]
  [(_ METHOD URI (equals THING))
   #'(begin
       (cmd METHOD URI #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (unless (equal-jsexprs? THING (last-response->jsexpr))
         (error (format "Last response does not equal ~a" (json-pretty-print THING)))))]
  [(_ METHOD URI (responds-with CODE))
   #'(begin
       (cmd METHOD URI #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE))]
  [(_ METHOD URI "times" "out")
   #'(begin
       (cmd METHOD URI #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1)))]
  [(_ METHOD PAYLOAD URI "times" "out")
   #'(begin
       (cmd/payload METHOD URI PAYLOAD #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1)))]
  [(_ METHOD URI (responds-with CODE) (equals THING))
   #'(begin
       (cmd METHOD URI #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE)
       (unless (equal-jsexprs? THING (last-response->jsexpr))
         (error (format "Last response does not equal ~a" (json-pretty-print THING)))))]
  [(_ METHOD PAYLOAD URI (responds-with CODE))
   #'(begin
       (cmd/payload METHOD URI PAYLOAD #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE))]
  [(_ METHOD PAYLOAD URI (responds-with CODE) (equals THING))
   #'(begin
       (cmd/payload METHOD URI PAYLOAD #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE)
       (unless (equal-jsexprs? THING (last-response->jsexpr))
         (error (format "Last response does not equal ~a" (json-pretty-print THING)))))]
  [(_ METHOD PAYLOAD URI (with-headers HEADERS) (responds-with CODE))
   #'(begin
       (unless (json-object? HEADERS)
         (error (format "Headers is not a JSON object! ~a" (pretty-print HEADERS))))
       (for ([(k v) (in-hash HEADERS)])
         (unless (string? v)
           (error (format "Value for property \"~a\" is not a string: ~a" k v))))
       (cmd/payload METHOD URI PAYLOAD #:headers HEADERS #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE))]
  [(_ METHOD PAYLOAD URI (with-headers HEADERS) (responds-with CODE) (equals THING))
   #'(begin
       (unless (json-object? HEADERS)
         (error (format "Headers is not a JSON object! ~a" (pretty-print HEADERS))))
       (for ([(k v) (in-hash HEADERS)])
         (unless (string? v)
           (error (format "Value for property \"~a\" is not a string: ~a" k v))))
       (cmd/payload METHOD URI PAYLOAD #:headers HEADERS #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (response-code-matches? CODE)
       (unless (equal-jsexprs? THING (last-response->jsexpr))
         (error (format "Last response does not equal ~a" (json-pretty-print THING)))))]
  [(_ METHOD PAYLOAD URI (positive-satisfies SCHEMA))
   #'(begin
       (unless (json-schema? SCHEMA)
         (error (format "1. Purported schema is not actually a JSON Schema:~a~a"
                        #\newline
                        (pretty-print SCHEMA))))
       (cmd/payload METHOD URI PAYLOAD #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (unless (adheres-to-schema? (last-response->jsexpr) SCHEMA)
         (error "Response does not satisfy schema.")))]
  [(_ METHOD PAYLOAD URI (negative-satisfies SCHEMA))
   #'(begin
       (unless (json-schema? SCHEMA)
         (error (format "2. Purported schema is not actually a JSON Schema:~a~a"
                        #\newline
                        (pretty-print SCHEMA))))
       (cmd/payload METHOD URI PAYLOAD #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (when (adheres-to-schema? (last-response->jsexpr) SCHEMA)
         (error "Response does satisfy schema!")))]
  [(_ METHOD PAYLOAD URI (with-headers HEADERS) (positive-satisfies SCHEMA))
   #'(begin
       (unless (json-object? HEADERS)
         (error (format "Headers is not a JSON object! ~a" (pretty-print HEADERS))))
       (for ([(k v) (in-hash HEADERS)])
         (unless (string? v)
           (error (format "Value for property \"~a\" is not a string: ~a" k v))))
       (unless (json-schema? SCHEMA)
         (error (format "3. Purported schema is not actually a JSON Schema:~a~a"
                        #\newline
                        (json-pretty-print SCHEMA))))
       (cmd/payload METHOD URI PAYLOAD #:headers HEADERS #:timeout (param-timeout))
       (when (last-request-failed?)
         (exit 1))
       (unless (adheres-to-schema? (send last-response as-json) SCHEMA)
         (error "Response does not satisfy schema.")))])

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
           (has-type name "is" "a" type)))]
    [(_ ident:string expr "(" adjective type ")")
     (with-syntax [(name (format-id #'ident "~a" (syntax->datum #'ident)))]
       #'(begin
           (define name expr)
           (has-type name "is" "a" adjective type)))]))


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
    [(_ expr "is" "a" "string")
     #'(unless (string? expr)
         (error (format "~a is not a string!" (render expr))))]
    [(_ expr "is" "null")
     #'(unless (eq? expr 'null)
         (error (format "~a is not null!" (render expr))))]
    [(_ expr "is" "non" "null")
     #'(when (eq? expr 'null)
         (error (format "~a is null!" (render expr))))]
    [(_ expr "is" "a" "number")
     #'(unless (number? expr)
         (error (format "~a is not a number!" (render expr))))]
    [(_ expr "is" "an" "integer")
     #'(unless (integer? expr)
         (error (format "~a is not an integer!" (render expr))))]
    [(_ expr "is" "an" "even" "integer")
     #'(begin
         (unless (integer? expr)
           (error (format "~a is not an integer!" (render expr))))
         (unless (even? expr)
           (error (format "~a is not even!" (render expr)))))]
    [(_ expr "is" "an" "odd" "integer")
     #'(begin
         (unless (integer? expr)
           (error (format "~a is not an integer!" (render expr))))
         (unless (odd? expr)
           (error (format "~a is not odd!" (render expr)))))]
    [(_ expr "is" "a" "positive" "integer")
     #'(begin
         (has-type expr "is" "an" "integer")
         (unless (> expr 0)
           (error (format "~a is not positive!" (render expr)))))]
    [(_ expr "is" "a" "negative" "integer")
     #'(begin
         (has-type expr "is" "an" "integer")
         (unless (< expr 0)
           (error (format "~a is not negative!" (render expr)))))]
    [(_ expr "is" "a" "non" "empty" "string")
     #'(begin
         (has-type expr "is" "a" "string")
         (unless (non-empty-string? expr)
           (error (format "~a is the empty string!" (render expr)))))]
    [(_ expr "is" "not" "a" "boolean")
     #'(when (boolean? expr)
         (error (format "~a is a boolean!" (render expr))))]
    [(_ expr "is" "not" "a" "string")
     #'(when (string? expr)
         (error (format "~a is a string!" (render expr))))]
    [(_ expr "is" "not" "an" "array")
     #'(when (list? expr)
         (error (format "~a is an array!" (render expr))))]
    [(_ expr "is" "not" "an" "integer")
     #'(when (integer? expr)
         (error (format "~a is an integer!" (render expr))))]
    [(_ expr "is" "not" "an" "object")
     #'(when (hash? expr)
         (error (format "~a is an object!" (render expr))))]
    [(_ expr "is" "not" "a" "positive" "integer")
     #'(when (and (integer? expr)
                  (> expr 0))
         (error (format "~a is not a positive integer!" (render expr))))]
    [(_ expr "is" "not" "a" "negative" "integer")
     #'(when (and (integer? expr)
                  (< expr 0))
         (error (format "~a is not a negative integer!" (render expr))))]
    [(_ expr "is" "not" "an" "even" "integer")
     #'(when (and (integer? expr)
                  (even? expr))
         (error (format "~a is not an even integer!" (render expr))))]
    [(_ expr "is" "not" "an" "odd" "integer")
     #'(when (and (integer? expr)
                  (odd? expr))
         (error (format "~a is not an odd integer!" (render expr))))]
    [(_ expr "is" "empty")
     #'(cond [(string? expr)
              (unless (string=? "" expr)
                (error (format "\"~a\" is a non-empty string (its value is \"~a\"." (render expr) (json-pretty-print expr))))]
             [(list? expr)
              (unless (empty? expr)
                (error (format "\"~a\" is a non-empty array (it is ~a)" (render expr) (json-pretty-print expr))))]
             [(hash? expr)
              (unless (hash-empty? expr)
                (error (format "\"~a\" is a non-empty object (is value is ~a)" (render expr) (json-pretty-print expr))))]
             [(number? expr)
              (error "\"~a\" refers to a number; it is neither empty nor non-empty." (render expr))]
             [(boolean? v)
              (error "\"~a\" refers to a boolean; it is neither empty nor non-empty." (render expr))]
             [(eq? 'null expr)
              (error (format "~a refers to the null value, which is neither empty nor non-empty." (render expr)))])]
    [(_ expr "is" "non" "empty")
     #'(let ([v expr])
         (cond [(string? v)
                (when (string=? "" v)
                  (error (format "~a is the empty string!" (render expr))))]
               [(list? v)
                (when (empty? v)
                  (error (format "~a is the empty array!" (render expr))))]
               [(hash? v)
                (when (hash-empty? v)
                  (error (format "~a is the empty object!" (render expr))))]
               [(number? v)
                (error "~a refers to a number; it is neither empty nor non-empty." (render expr))]
               [(boolean? v)
                (error "~a refers to a boolean; it is neither empty nor non-empty." (render expr))]
               [(eq? 'null v)
                (error (format "~a refers to the null value, which is neither empty nor non-empty." (render expr)))]))]
    [(_ expr "is" "positive")
     #'(begin
         (has-type expr "is" "a" "number")
         (unless (> expr 0)
           (error (format "~a is not positive!" (render expr)))))]
    [(_ expr "is" "non" "positive")
     #'(begin
         (has-type expr "is" "a" "number")
         (when (> expr 0)
           (error (format "~a is not positive!" (render expr)))))]
    [(_ expr "is" "negative")
     #'(begin
         (has-type expr "is" "a" "number")
         (unless (< expr 0)
           (error (format "~a is not negative!" (render expr)))))]
    [(_ expr "is" "non" "negative")
     #'(begin
         (has-type expr "is" "a" "number")
         (when (< expr 0)
           (error (format "~a is negative!" (render expr)))))]))

(define-syntax (json-pointer stx)
  (syntax-parse stx
    [(_ jp:string)
     #'(json-pointer-value jp (last-response->jsexpr))]))

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

(define-macro-cases schema-ref
  [(_ "in" PATH)
   #'(begin
       (let* ([full-path (expand-path PATH)]
              [bs (file-content/bytes (expand-path PATH))])
         (with-handlers ([exn:fail:contract? (lambda (e)
                                               (error (format "Content of ~a is malformed UTF-8." (path->string full-path))))])
           (let ([s (bytes->string/utf-8 bs)])
             (with-handlers ([exn:fail? (lambda (e)
                                          (error (format "Content of ~a is malformed JSON." (path->string full-path))))])
               (string->jsexpr s))))))]
  [(_ E)
   #'E])

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
     #'(unless (json-pointer-exists? jp)
         (error (format "JSON Pointer ~a does not exist." jp)))]
    [(_ jp:string "does" "not" "exist")
     #'(when (json-pointer-exists? jp)
         (error (format "JSON Pointer ~a exists!" jp)))]
    [(_ jp:string "exists" "and" "is" "empty")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "empty"))]
    [(_ jp:string "exists" "and" "is" "non" "empty")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "non" "empty"))]
    [(_ jp:string "exists" "and" "is" "positive")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "positive"))]
    [(_ jp:string "exists" "and" "is" "non" "positive")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "non" "positive"))]
    [(_ jp:string "exists" "and" "is" "negative")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "negative"))]
    [(_ jp:string "exists" "and" "is" "non" "negative")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "non" "negative"))]
    [(_ jp:string "exists" "and" "is" "a" "non" "empty" "string")
     #'(begin
         (jp-existence jp "exists")
         (has-type (json-pointer jp) "is" "a" "non" "empty" "string"))]))

(define-syntax (equality stx)
  (syntax-parse stx
    [(_ lhs rhs)
     #`(unless (equal-jsexprs? lhs rhs)
         (error (format "Equation fails: ~a is ~a, but ~a is ~a."
                        (render lhs)
                        (json-pretty-print lhs)
                        (render rhs)
                        (json-pretty-print rhs))))]))

(define-syntax (disequality stx)
  (syntax-parse stx
    [(_ lhs rhs)
     #`(when (equal-jsexprs? lhs rhs)
         (error (format "Equation is true: ~a is ~a and ~a is ~a."
                        (render lhs)
                        (json-pretty-print lhs)
                        (render rhs)
                        (json-pretty-print rhs))))]))

(define-macro-cases inequality
  [(_ E1 "<" E2)
   #'(unless (< E1 E2)
       (error (format "~a is not less than ~a!" (render E1) (render E2))))]
  [(_ E1 "<=" E2)
   #'(unless (<= E1 E2)
       (error (format "~a is not less than or equal to ~a!" (render E1) (render E2))))]
  [(_ E1 ">" E2)
   #'(unless (> E1 E2)
       (error (format "~a is not greater than ~a!" (render E1) (render E2))))]
  [(_ E1 ">=" E2)
   #'(unless (> E1 E2)
       (error (format "~a is not greater or equal to ~a!" (render E1) (render E2))))])

(define-syntax (header-presence stx)
  (syntax-parse stx
    [(_ header:string "present")
     #'(when (eq? #f (fetch-response-header header))
         (error (format "Header ~a missing from previous response." header)))]
    [(_ header:string "absent")
     #'(unless (eq? #f (fetch-response-header header))
         (error (format "Header ~a present in previous response." header)))]))

(define-syntax (response-head-id stx)
  (syntax-parse stx
    [(_ h:string)
     #'(cond
         [(response-received?)
          (match (fetch-response-header h)
            [(? string? s)
             s]
            [else
             (error (format "Previous response does not have header \"~a\"." h))])]
         [else
          (error (format "No response received yet; cannot look for response header \"~a\"." h))])]))

(define-syntax (sequence-predicate stx)
  (syntax-parse stx
    [(_ e1 "starts" "with" e2)
     #'(begin
         (unless (or (string? e1) (list? e1))
           (error (format "~a holds neither a string nor an array!" (render e1))))
         (unless (or (string? e2) (list? e2))
           (error (format "~a holds neither a string nor an array!" (render e2))))
         (unless (starts-with? e1 e2)
           (error (format "~a does not start with ~a" (render e1) (render e2)))))]
    [(_ e1 "ends" "with" e2)
     #'(begin
         (unless (or (string? e1) (list? e1))
           (error (format "~a holds neither a string nor an array!" (render e1))))
         (unless (or (string? e2) (list? e2))
           (error (format "~a holds neither a string nor an array!" (render e2))))
         (unless (ends-with? e1 e2)
           (error (format "~a (~a) does not end with ~a (~a)." (render e1) e1 (render e2) e2))))]))

(define-macro-cases echo
  [(_)
   #'(begin
       (unless (response-received?)
         (error "No response has been received yet! Cannot echo."))
       (define body (with-handlers ([exn:fail:contract? (const "# (response body is invalid UTF-8!)")])
                      (bytes->string/utf-8 (get-last-response-body/raw))))
       (displayln (comment-out-lines body)))]
  [(_ (json-pointer JP))
   #'(begin
       (unless (response-received?)
         (error (format "No response has been received yet! Cannot echo JSON Pointer ~a." JP)))
       (unless (json-pointer-exists? JP)
         (error (format "JSON Pointer ~a does not exist!" JP)))
       (displayln (format "~a = ~a" (render JP) (json-pretty-print (fetch-json-pointer-value JP)))))]
  [(_ (normal-identifier ID))
   (with-syntax [(name (format-id #'ID "~a" (syntax->datum #'ID)))]
     #'(displayln (format "~a = ~a" (render (normal-identifier ID)) (json-pretty-print name))))]
  [(_ (response-head-id ID))
   #'(begin
       (unless (response-received?)
         (error (format "No response has been received yet! Cannot echo response header \"~a\"." ID)))
       (unless (response-header-exists? ID)
         (error (format "Response does not contain header \"~a\"." ID)))
       (displayln (format "~a = ~a" (render (response-head-id ID)) (fetch-response-header ID))))])

(define-macro-cases exec
  [(_ FILENAME)
   #'(begin
       (unless (file-exists? FILENAME)
         (error (format "No such file: ~a" FILENAME)))
       (define success? #f)
       (define output
         (parameterize ([current-input-port (open-input-bytes #"")])
           (with-output-to-bytes
             (lambda ()
               (set! success? (system* FILENAME))))))
       (unless success?
         (error (format "Failed to execute ~a with no arguments!" FILENAME)))
       (update-last-response! #f #f output))]
  [(_ FILENAME "[" ARGS ... "]")
   #'(begin
       (unless (file-exists? FILENAME)
         (error (format "No such file: ~a" FILENAME)))
       (define success? #f)
       (define output
         (parameterize ([current-input-port (open-input-bytes #"")])
           (with-output-to-bytes
             (lambda ()
               (set! success? (system* FILENAME ARGS ...))))))
       (unless success?
         (error (format "Failed to execute ~a with no arguments!" FILENAME)))
       (update-last-response! #f #f output))]
  [(_ FILENAME (normal-identifier ID))
   #'(begin
       (unless (file-exists? FILENAME)
         (error (format "No such file: ~a" FILENAME)))
       (unless (list? (normal-identifier ID))
         (error (format "We need a list, but ~a isn't one." (render (normal-identifier ID)))))
       (unless (andmap string? (normal-identifier ID))
         (error (format "We need a list of strings, but ~a isn't one." (render (normal-identifier ID)))))
       (define success? #f)
       (define output
         (parameterize ([current-input-port (open-input-bytes #"")])
           (with-output-to-bytes
             (lambda ()
               (set! success? (apply system* FILENAME (normal-identifier ID)))))))
       (unless success?
         (error (format "Failed to execute ~a with no arguments!" FILENAME)))
       (update-last-response! #f #f output))])

(define-macro (exec-arg-item ITEM)
  #'ITEM)

(define-syntax (bang stx)
  (syntax-parse stx
    [(_ n:integer)
     #'(let ([num-received (count-received-responses)])
         (when (< num-received n)
           (error (format "We have received ~a responses, so we cannot look at response number ~a" num-received n)))
         (let ([received (list-ref (unbox responses) (sub1 n))])
           (match (list-ref (unbox responses) (sub1 n))
             [(? bytes? b)
              (bytes->jsexpr b)]
             [(? object? o)
              (send o as-jsexpr)])))]))

(provide bang)

(define-macro-cases check-environment-variables
  ; the first two cases are the nut of the whole thing; everything else is just
  ; breaking the program up, recursively hunting for references to environment
  ; variables:
  [(_ (env-identifier ENV))
   #'(unless (string? (getenv ENV))
       (error (format "Environment variable ~a is not set!" ENV)))]
  [(_ (env-identifier ENV FALLBACK))
   #'(void)]

  ; the remaining cases just break the program up:
  [(_ (riposte-program))
   #'(void)]
  [(_ (riposte-program S STEP ...))
   #'(begin
       (check-environment-variables S)
       (check-environment-variables (riposte-program STEP ...)))]
  [(_ (json-array))
   #'(void)]
  [(_ (json-array ITEM ITEMS ...))
   #'(begin
       (check-environment-variables ITEM)
       (check-environment-variables (json-array ITEMS ...)))]
  [(_ (json-array-item VALUE))
   #'(check-environment-variables VALUE)]
  [(_ (json-object))
   #'(void)]
  [(_ (json-object ITEM ITEMS ...))
   #'(begin
       (check-environment-variables ITEM)
       (check-environment-variables (json-object ITEMS ...)))]
  [(_ (json-object-item KEY VALUE))
   #'(check-environment-variables VALUE)]
  [(_ (json-boolean B))
   #'(void)]
  [(_ (parameter-assignment PARAM VAL))
   #'(check-environment-variables VAL)]
  [(_ (uri-template T ...))
   #'(void)]

  ; the command forms:
  [(_ (command METHOD URI))
   #'(check-environment-variables URI)]
  [(_ (command METHOD URI (responds-with CODE)))
   #'(check-environment-variables URI)]
  [(_ (command METHOD PAYLOAD URI (responds-with CODE)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI))]
  [(_ (command METHOD PAYLOAD URI (with-headers HEADERS)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables HEADERS)
       (check-environment-variables URI))]
  [(_ (command METHOD PAYLOAD URI (with-headers HEADERS) (responds-with CODE)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables HEADERS)
       (check-environment-variables URI))]
  [(_ (command METHOD PAYLOAD URI (positive-satisfies SCHEMA)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI)
       (check-environment-variables SCHEMA))]
  [(_ (command METHOD PAYLOAD URI (negative-satisfies SCHEMA)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI)
       (check-environment-variables SCHEMA))]
  [(_ (command METHOD URI (equals THING)))
   #'(begin
       (check-environment-variables URI)
       (check-environment-variables THING))]
  [(_ (command METHOD PAYLOAD URI (equals THING)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI)
       (check-environment-variables THING))]
  [(_ (command METHOD URI (responds-with CODE) (equals THING)))
   #'(begin
       (check-environment-variables URI)
       (check-environment-variables THING))]
  [(_ (command METHOD PAYLOAD URI (responds-with CODE) (equals THING)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI)
       (check-environment-variables THING))]
  [(_ (command METHOD PAYLOAD URI (with-headers HEADS) (responds-with CODE) (equals THING)))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI)
       (check-environment-variables HEADS)
       (check-environment-variables THING))]
  [(_ (command METHOD URI "times" "out"))
   #'(check-environment-variables URI)]
  [(_ (command METHOD PAYLOAD URI "times" "out"))
   #'(begin
       (check-environment-variables PAYLOAD)
       (check-environment-variables URI))]
  [(_ (responds-with CODE))
   #'(void)]
  [(_ (schema-ref S))
   #'(check-environment-variables S)]
  [(_ (schema-ref "in" FILE))
   #'(void)]
  [(_ (jp-existence WHATEVER ...))
   #'(void)]
  [(_ (normal-assignment VAR VAL "(" TYPE ")"))
   #'(check-environment-variables VAL)]
  [(_ (normal-assignment VAR VAL "(" ADJECTIVE TYPE ")"))
   #'(check-environment-variables VAL)]
  [(_ (normal-assignment VAR VAL "(" "non" ADJECTIVE TYPE ")"))
   #'(check-environment-variables VAL)]
  [(_ (normal-assignment VAR VAL))
   #'(check-environment-variables VAL)]
  [(_ (header-assignment H V))
   #'(check-environment-variables V)]
  [(_ (expression E))
   #'(check-environment-variables E)]
  [(_ (expression E1 "+" E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (expression E1 "-" E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (expression E1 "*" E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (json-pointer JP))
   #'(void)]
  [(_ (normal-identifier ID))
   #'(void)]
  [(_ (header-presence HEAD ABSENT-OR-PRESENT))
   #'(void)]
  [(_ (sequence-predicate E1 "starts" "with" E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (sequence-predicate E1 "ends" "with" E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (response-head-id H))
   #'(void)]
  [(_ (has-type E "is" "non" WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "a" WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "a" ADJECTIVE WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "a" "non" ADJECTIVE WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "an" ADJECTIVE WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "not" "a" WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "not" "a" ADJECTIVE WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "not" "an" WHATEVER))
   #'(check-environment-variables E)]
  [(_ (has-type E "is" "not" "an" ADJECTIVE WHATEVER))
   #'(check-environment-variables E)]
  [(_ (equality LHS RHS))
   #'(begin
       (check-environment-variables LHS)
       (check-environment-variables RHS))]
  [(_ (disequality LHS RHS))
   #'(begin
       (check-environment-variables LHS)
       (check-environment-variables RHS))]
  [(_ (inequality E1 REL E2))
   #'(begin
       (check-environment-variables E1)
       (check-environment-variables E2))]
  [(_ (echo))
   #'(void)]
  [(_ (echo WHATEVER))
   #'(void)]
  [(_ (exec FILE))
   #'(check-environment-variables FILE)]
  [(_ (exec FILE (normal-identifier ID)))
   #'(check-environment-variables (normal-identifier ID))]
  [(_ (exec FILE "[" "]"))
   #'(void)]
  [(_ (exec FILE "[" ARG ARGS ... "]"))
   #'(begin
       (check-environment-variables ARG)
       (check-environment-variables (exec FILE "[" ARGS ... "]")))]
  [(_ (exec-arg-item I))
   #'(check-environment-variables I)]
  [(_ (bang B))
   #'(void)]
  [else
   (syntax-parse caller-stx
     [(_ s:string)
      #'(void)]
     [(_ n:number)
      #'(void)]
     [else
      (error (format "We are really down on our luck here with ~a" caller-stx))])])

(define-macro (riposte-repl FORM)
  #'FORM)

(define-macro (program-step S)
  #'S)
