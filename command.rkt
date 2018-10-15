#lang racket/base

(provide make-command-expression
         command-expression?)

(require racket/class
         racket/dict
         racket/undefined
         racket/contract
         racket/hash
         racket/match
         racket/list
         racket/function
         brag/support
         net/url
         http
         ejs
         uri-template
         json-pointer
         argo
         (only-in racket/format
                  ~a)
         (file "step.rkt")
         (file "util.rkt")
         (file "environment.rkt")
         (only-in (file "expression.rkt")
                  expression?)
         (only-in (file "identifier.rkt")
                  variable-identifier-expression?))

(module+ test
  (require rackunit))

(define/contract (render-code code)
  (any/c . -> . string?)
  (match code
    [(? string?)
     code]
    [(list 'http-response-code c)
     c]
    [else
     (format "~a" code)]))

(module+ test
  (check-equal? "403"
               (render-code "403"))
  (check-equal? "403"
               (render-code '(http-response-code "403")))
  (check-equal? "4XX"
               (render-code '(http-response-code "4XX"))))

(define (http-response-code? x)
  (match x
    [(? exact-positive-integer?)
     #t]
    [(list 'http-response-code s)
     (string? s)]
    [else
     #f]))

(define/contract (response-code-matches? received-code expected-code)
  (exact-positive-integer? list? . -> . boolean?)
  (match expected-code
    [(list 'http-response-code code)
     (response-code-matches-pattern (format "~a" received-code)
                                    code)]))

(define/contract (response-code-matches-pattern received-code expected-code)
  (string? string? . -> . boolean?)
  (cond ((string=? "" received-code)
         (string=? "" expected-code))
        ((string=? "" expected-code)
         #f)
        (else
         (define c1 (string-ref received-code 0))
         (define c2 (string-ref expected-code 0))
         (cond ((or (char=? c2 #\X)
                    (char=? c2 #\x))
                (response-code-matches-pattern (substring received-code 1)
                                               (substring expected-code 1)))
               ((char=? c1 c2)
                (response-code-matches-pattern (substring received-code 1)
                                               (substring expected-code 1)))
               (else
                #f)))))

(define/contract (read-entity/bytes+response-code in h)
  (input-port? string? . -> . (list/c exact-integer? string? bytes?))
  (list (extract-http-code h)
        h
        (read-entity/bytes in h)))

(define/contract (request method url headers)
  (string?
   string?
   (or/c false/c
         (hash/c symbol? string?))
   . -> .
   (list/c exact-integer? string? bytes?))
  (define final-headers
    (cond [(eq? headers #f)
           (hasheq)]
          [else
           headers]))
  (with-handlers ([exn:fail:network? (lambda (e)
                                       (error (format "Failed to connect to ~a!" url)))])
    (call/input-request "1.1"
                         method
                         url
                         final-headers
                         read-entity/bytes+response-code)))

(define/contract (request/payload method url headers payload)
  (string?
   string?
   (or/c false/c
         (hash/c symbol? string?))
   ejsexpr?
   . -> .
   (list/c exact-integer? string? bytes?))
  (define final-headers
    (cond [(eq? headers #f)
           (hasheq)]
          [else
           headers]))
  (log-error "payload: ~a" payload)
  (with-handlers ([exn:fail:network? (lambda (e)
                                       (error (format "Failed to connect to ~a!" url)))])
    (call/output-request "1.1"
                         method
                         url
                         (ejsexpr->bytes payload)
                         #f
                         final-headers
                         read-entity/bytes+response-code)))

(define command-expression%
  (class step%
    (super-new)
    (init-field method
                uri)
    (field (payload undefined)
           (headers undefined))
    (define/public (set-payload! p)
      (unless (variable-identifier-expression? p)
        (error "Payload should be either an ejsexpr? or an expression? value."))
      (set! payload p))
    (define/public (set-headers! h)
      (unless (variable-identifier-expression? h)
        (error "Headers should be either a variable!"))
      (set! headers h))
    (define/override (render)
      (cond [(variable-identifier-expression? payload)
             (cond [(variable-identifier-expression? headers)
                    (format "~a ~a with headers ~a"
                            method
                            uri
                            (send headers render))]
                   [else
                    (format "~a ~a ~a"
                            method
                            (send payload render)
                            uri)])
             (format "~a ~a ~a" method (send payload render) uri)]
            [(variable-identifier-expression? headers)
             (format "~a ~a ~a"
                     method
                     uri
                     (send headers render))]
            [else
             (format "~a ~a" method uri)]))
    (define/override (evaluate env)
      (define ass (environment->assignment (environment-table env)))
      (define url (expand-template uri ass))
      (when (global-variable-set? env 'base-url)
        (set! url (url->string (combine-url/relative (string->url (lookup-global-variable env 'base-url))
                                                     url))))
      (define hs (environment-header-table env))
      (when (expression? headers)
        (define h (send headers evaluate env))
        (unless (hash? h)
          (error (format "Evaluating ~a yielded a non-hash: ~a" headers h)))
        (set! hs (hash-union headers h)))
      (displayln (format "~a ~a ..."
                         method
                         uri))
      (define code+headers+response
        (cond [(expression? payload)
               (request/payload method
                                url
                                hs
                                (send payload evaluate env))]
              [else
               (request method url hs)]))
      (define code (first code+headers+response))
      (displayln (~a code))
      (define response-headers (second code+headers+response))
      (define response/bytes (third code+headers+response))
      (define response/jsexpr (with-handlers ([exn:fail? (lambda (e)
                                                           (log-error "response body is busted as JSON: ~a" response/bytes)
                                                           #f)])
                                (bytes->ejsexpr response/bytes)))
      (log-error "response as jsexpr: ~a" response/jsexpr)
      (struct-copy environment
                   env
                   [response response/jsexpr]
                   [response-headers (heads-string->dict response-headers)]
                   [response-code code]))))

(define/contract (command-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x command-expression%)))

(define (make-command-expression m
                                 u
                                 #:payload [p #f]
                                 #:headers [h #f])
  (->* (string? string?)
       (#:payload (or/c false/c variable-identifier-expression?)
        #:headers (or/c false/c variable-identifier-expression?))
       command-expression?)
  (define c (new command-expression%
                 [method m]
                 [uri u]))
  (when (variable-identifier-expression? p)
    (send c set-payload! p))
  (when (variable-identifier-expression? h)
    (send c set-headers! h))
  c)

(define (command? x)
  (and (object? x)
       (is-a? x command-expression%)))
