#lang racket/base

(provide cmd
         cmd/payload
         request-headers
         response-code-matches?
         response-received?
         last-response
         param-base-url
         param-timeout
         last-response->jsexpr
         fetch-json-pointer-value
         json-pointer-exists?
         json-pointer-does-not-exist?
         fetch-response-header
         response-header-exists?
         get-last-response-body/raw)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         racket/contract
         racket/dict
         racket/match
         racket/hash
         net/url
         http/request
         http/head
         json
         json-pointer
         (only-in racket/class
                  send)
         (only-in racket/function
                  const)
         (only-in misc1/syntax
                  with-output-string)
         (only-in argo
                  json-pretty-print)
         (file "version.rkt")
         (file "response.rkt")
         (file "json.rkt"))

(define param-base-url (make-parameter #f))

(define param-timeout (make-parameter #f))

; string? string? -> void
(define (cmd method url)
  (define final-url
    (cond [(url? (param-base-url))
           (url->string (combine-url/relative (param-base-url) url))]
          [else
           url]))
  (display (format "~a ~a" method final-url))
  (flush-output)
  (define result (request method final-url request-headers))
  (match result
    [(list code headers body)
     (displayln (format " responds with ~a" code))
     (update-last-response! code headers body)]))

(define/contract (request/payload method url headers payload)
  (string?
   string?
   (or/c false/c
         (hash/c symbol? string?))
   jsexpr?
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
    (call/output-request "1.1"
                         method
                         url
                         (jsexpr->bytes payload)
                         #f
                         headers
                         read-entity/bytes+response-code)))

(define/contract (cmd/payload method url payload #:headers [additional-headers (hash)])
  (->* (string? string? jsexpr?)
       (#:headers (hash/c symbol? string?))
       void)
  (define headers (hash-union (make-immutable-hasheq (hash->list request-headers))
                              additional-headers))
  (define final-url
    (cond [(url? (param-base-url))
           (url->string (combine-url/relative (param-base-url) url))]
          [else
           url]))
  (display (format "~a ~a" method final-url))
  (flush-output)
  (define result (request/payload method final-url headers payload))
  (match result
    [(list code response-headers body)
     (displayln (format " responds with ~a" code))
     (update-last-response! code response-headers body)]))

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
  (unless (response-received?)
    (error "No response has been received yet, so we cannot check whether response code matches \"~a\"."
           code))
  (define matches?
    (response-code-matches-pattern? (format "~a" (send last-response get-code))
                                    code))
  (unless matches?
    (error
     (format "The received response has code ~a, but we expected ~a."
             (send last-response get-code)
             code))))

; string? string? (#f hash? symbol? string?) -> (list/c exact-integer? (and/c immutable? (hash/c symbol? string?)) bytes?)
(define (request method url headers)
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

(define/contract
  last-response
  (box/c (or/c false/c response? bytes?))
  (box #f))

(define/contract (response-received?)
  (-> boolean?)
  (not (eq? #f (unbox last-response))))

(define/contract (response-has-body?)
  (-> boolean?)
  (match (unbox last-response)
    [#f
     #f]
    [(? bytes?)
     #t]
    [(? response? r)
     (send r has-body?)]))

(define/contract (response-well-formed?)
  (-> boolean?)
  (match (unbox last-response)
    [#f
     #f]
    [(? bytes? b)
     (with-handlers ([exn:fail:contract? (const #f)])
       (bytes->jsexpr b)
       #t)]
    [(? response? r)
     (send r body-is-well-formed?)]))

(define/contract (last-response->jsexpr)
  (-> jsexpr?)
  (cond [(not (response-received?))
         (error "No response received; cannot convert it to JSON.")]
        [(not (response-has-body?))
         (error "Previous response has an empty body; cannot convert it to JSON.")]
        [(not (response-well-formed?))
         (error "Previous response has a malformed body.")]
        [else
         (match (unbox last-response)
           [#f
            (error "Strange: We've say we've received a response, but we don't have it. Please file a bug.")]
           [(? bytes? b)
            (bytes->jsexpr b)]
           [(? response? r)
            (send r as-jsexpr)])]))

(define (fetch-json-pointer-value jp)
  (cond [(not (response-received?))
         (error (format "No response received; cannot evaluate JSON Pointer \"~a\"." jp))]
        [(not (response-has-body?))
         (error (format "Previous response has an empty body; cannot evaluate JSON Pointer \"~a\"." jp))]
        [(not (response-well-formed?))
         (error (format "Previous response has a malformed body; cannot evaluate JSON Pointer \"~a\"." jp))]
        [else
         (with-handlers ([exn:fail? (const (void))])
           (json-pointer-value jp (last-response->jsexpr)))]))

(define (json-pointer-exists? jp)
  (not (void? (fetch-json-pointer-value jp))))

(define (json-pointer-does-not-exist? jp)
  (cond [(not (response-received?))
         (error (format "No response received; cannot evaluate JSON Pointer \"~a\"." jp))]
        [(not (response-has-body?))
         (error (format "Previous response has an empty body; cannot evaluate JSON Pointer \"~a\"." jp))]
        [(not (response-well-formed?))
         (error (format "Previous response has a malformed body; cannot evaluate JSON Pointer \"~a\"." jp))]
        [else
         (define js (last-response->jsexpr))
         (cond [(or (json-object? js)
                    (json-array? js))
                (define val
                  (with-handlers ([exn:fail? (lambda (err)
                                               (void))])
                    (json-pointer-value jp js)))
                (cond [(void? val)
                       (void)]
                      [else
                       (define msg (with-output-string
                                     (displayln (format "JSON Pointer \"~a\" does exist. The value is:" jp))
                                     (displayln (json-pretty-print val))))
                       (error msg)])]
               [else
                (define msg (with-output-string
                              (displayln (format "The previous response is neither an array nor an object, so we cannot evaluate JSON Pointer \"~a\"." jp))
                              (displayln "Previous response was:")
                              (displayln (json-pretty-print js))))
                  (error msg)])]))

(define/contract (update-last-response! code headers body)
  ((integer-in 100 599) (and/c immutable? (hash/c symbol? string?)) bytes? . -> . void)
  (set-box! last-response
            (make-response code headers body)))

(define (get-response-headers)
  (match (unbox last-response)
    [#f
     (error "Cannot fetch response headers because we haven't received a response yet!")]
    [(? bytes? b)
     (error "Cannot fetch response headers because we just executed an external program.")]
    [(? response? r)
     (send last-response get-headers)]))

(define (get-last-response-body/raw)
  (match (unbox last-response)
    [#f #f]
    [(? bytes? b)
     b]
    [(? response? r)
     (send r get-body/raw)]))

(define (fetch-response-header name)
  (define headers (get-response-headers))
  (define k (string->symbol (string-downcase name)))
  (hash-ref headers k #f))

(define (response-header-exists? name)
  (string? (fetch-response-header name)))

(define/contract (read-entity/bytes+response-code in h)
  (input-port? string? . -> . (list/c exact-integer? dict? bytes?))
  (list (extract-http-code h)
        (heads-string->dict h)
        (read-entity/bytes in h)))

(define/contract
  default-request-headers
  (and/c immutable? (hash/c symbol? string?))
  (hasheq 'User-Agent (format "Riposte/~a (https://riposte.in)" riposte-version)))

(define/contract
  request-headers
  (and/c (hash/c symbol? string?)
         (not/c immutable?))
  (make-hasheq (hash->list default-request-headers)))
