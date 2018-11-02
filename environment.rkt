#lang racket/base

(provide environment->assignment
         extend-environment/global
         extend-environment/header
         extend-environment
         lookup-header
         lookup-variable
         lookup-environment-variable
         lookup-global-variable
         global-variable-set?
         make-fresh-environment
         known-global-variable?)

(require (prefix-in uri-template:
                    uri-template)
         racket/contract
         racket/function
         racket/match
         ejs
         net/url
         (file "./value.rkt"))

(module+ test
  (require rackunit))

(struct environment
  (header-table
   table
   response
   response-headers
   response-code
   global-table)
  #:transparent)

(define (table? x)
  (and (hash? x)
       (andmap symbol? (hash-keys x))))

(define/contract (value->assignment-value v)
  (value? . -> . uri-template:value?)
  (cond ((number? v)
         (cond ((integer? v)
                (format "~a" v))
               ((rational? v)
                (format "~a" (exact->inexact v)))
               (else
                (error "Cannot convert number ~a to a value for a URI Template assignments." v))))
        ((string? v)
         v)
        ((eq? v 'null)
         'null)
        ((boolean? v)
         (error "Cannot convert booleans into URI Template assignments."))
        ((list? v)
         (map value->assignment-value v))
        ((hash? v)
         (for/list ([k (hash-keys v)])
           (list (symbol->string k)
                 (value->assignment-value (hash-ref v k)))))))

(define (convertible-value? x)
  (with-handlers ([exn:fail? (const #f)])
    (begin0
        #t
      (value->assignment-value x))))

(module+ test
  (check-true (convertible-value? 4))
  (check-false (convertible-value? -1.32))
  (check-true (convertible-value? 1/3))
  (check-false (convertible-value? #t))
  (check-false (convertible-value? #f))
  (check-true (convertible-value? 'null))
  (check-true (convertible-value? "hi"))
  (check-true (convertible-value? ""))
  (check-false (convertible-value? #""))
  (check-true (convertible-value? (list)))
  (check-true (convertible-value? (list "what" " the" "frack!")))
  (check-true (convertible-value? (hasheq 'hi "there"))))

(define/contract (environment->assignment env)
  ((hash/c symbol? value?) . -> . uri-template:assignment?)
  (for/hash ([i (hash-keys env)]
             #:when (convertible-value? (hash-ref env i)))
    (values (symbol->string i)
            (value->assignment-value (hash-ref env i)))))

(module+ test
  (let ([env (hasheq 'c 1)])
    (check-equal? (hash "c" "1")
                  (environment->assignment env))))


(define/contract (extend-environment env var val)
  (environment? symbol? value? . -> . environment?)
  (struct-copy environment
               env
               [table (hash-set (environment-table env)
                                var
                                val)]))

(define/contract (extend-environment/header env var val)
  (environment? symbol? value? . -> . environment?)
  (struct-copy environment
               env
               [header-table
                (hash-set (environment-header-table env)
                          var
                          val)]))

(define default-timeout 10)

(define/contract (set-timeout env timeout)
  (environment? (or/c exact-integer? false/c) . -> . environment?)
  (when (and (exact-integer? timeout)
             (< timeout 0))
    (error (format "Timeout should be a non-negative integer; ~a given." timeout)))
  (define table (environment-global-table env))
  (struct-copy environment
               env
               [global-table
                (cond [(exact-integer? timeout)
                       (hash-set table
                                 'timeout
                                 timeout)]
                      [else
                       (hash-remove table 'timeout)])]))

(define/contract (set-base-url env base)
  (environment? (or/c string? false/c) . -> . environment?)
  (define table (environment-global-table env))
  (struct-copy environment
               env
               [global-table
                (cond [(string? base)
                       (hash-set table
                                 'base-url
                                 base)]
                      [else
                       (hash-remove table 'base-url)])]))

(define/contract (extend-environment/global env var val)
  (environment? string? value? . -> . environment?)
  (match var
    ["timeout"
     (set-timeout env val)]
    ["base"
     (set-base-url env val)]
    [else
     (error (format "Unknown global variable ~a" var))]))

(define/contract (lookup-environment-variable var)
  (string? . -> . string?)
  (define val (getenv var))
  (unless (string? val)
    (error (format "Variable ~a missing from the environment." var)))
  val)

(define/contract (global-variable-set? env var)
  (environment? symbol? . -> . boolean?)
  (hash-has-key? (environment-global-table env) var))

(define/contract (lookup-global-variable env var)
  (environment? symbol? . -> . (or/c string? #f))
  (hash-ref (environment-global-table env)
            var
            #f))

(define/contract (lookup-variable var env)
  (string? environment? . -> . value?)
  (define table (environment-table env))
  (hash-ref table
            (string->symbol var)))

(define/contract (lookup-header var env)
  (string? environment? . -> . value?)
  (define table (environment-header-table env))
  (hash-ref table
            (string->symbol var)))

(define/contract (make-fresh-environment)
  (-> environment?)
  (environment (hash)
               (hash)
               #f
               #f
               #f
               (hash 'timeout default-timeout)))

(define global-variables
  (list "base"
        "timeout"))

(define/contract (known-global-variable? var)
  (string? . -> . boolean?)
  (list? (member var global-variables)))

(provide
 (contract-out
  [struct environment
    ([header-table
      (hash/c symbol? string?)]
     [table
      (hash/c symbol? ejsexpr?)]
     [response
      (or/c false/c ejsexpr?)]
     [response-headers
      (or/c false/c (hash/c symbol? string?))]
     [response-code
      (or/c false/c
            (integer-in 100 599))]
     [global-table
      (hash/c symbol? ejsexpr?)])]))
