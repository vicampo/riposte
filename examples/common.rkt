#lang racket/base

(provide run
         dispatch-rules
         make-header
         request->jsexpr)

(require racket/list
         racket/match
         racket/contract
         (only-in racket/function
                  const)
         json
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/http/cookie
         web-server/servlet-env
         web-server/dispatch
         net/cookie)

(module+ test
  (require rackunit))

(define (response/jsexpr
         jsexpr
         #:code [code 200]
         #:message [message #f]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type #"application/json;charset=utf-8"]
         #:cookies [cooks empty]
         #:headers [hdrs empty])
  (response/full
   code
   message
   seconds
   mime-type
   (append hdrs (map cookie->header cooks))
   (list (jsexpr->bytes jsexpr #:null (json-null)))))

(define (response/empty
         #:code [code 200]
         #:message [message #f]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type #""]
         #:cookies [cooks empty]
         #:headers [hdrs empty])
  (response/full
   code
   message
   seconds
   mime-type
   (append hdrs (map cookie->header cooks))
   (list)))

(define (not-found req)
  (response/empty #:code 404))

(define (not-allowed req)
  (response/empty #:code 405))

(define content-type-regexp
  (byte-regexp #"[Cc][Oo][Nn][Tt][Ee][Nn][Tt][-][Tt][Yy][Pp][Ee]"))

(define (has-json-mime-header req)
  (match (headers-assq* #"Content-Type" (request-headers/raw req))
    [#f
     #f]
    [(? header? h)
     (regexp-match? content-type-regexp (header-value h))]))

(define (bytes->string bs)
  (with-handlers ([exn:fail:contract? (const #f)])
    (begin0
        (bytes->string/utf-8 bs))))

(define (run servlet #:port [port 12345])
  (serve/servlet servlet
                 #:port port
                 #:stateless? #t
                 #:command-line? #t
                 #:servlet-regexp (regexp ".*")))

(define (request->jsexpr req)
  (with-handlers ([exn:fail:contract? (const (void))]
                  [exn? (lambda (err)
                          (log-error "Unexpected non-contract error: ~a" (exn-message err))
                          (void))])
    (bytes->jsexpr (request-post-data/raw req))))

(define (get-request-header req id)
  (match (headers-assq* id (request-headers/raw req))
    [#f #f]
    [(? header? h)
     (header-value h)]))

(provide/contract
 [response/jsexpr
  ((jsexpr?)
   (#:code response-code/c
    #:message (or/c #f bytes?)
    #:seconds real?
    #:mime-type (or/c #f bytes?)
    #:cookies (listof cookie?)
    #:headers (listof header?))
   . ->* . response?)]
 [response/empty
  (()
   (#:code response-code/c
    #:message (or/c #f bytes?)
    #:seconds real?
    #:mime-type (or/c #f bytes?)
    #:cookies (listof cookie?)
    #:headers (listof header?))
   . ->* . response?)]
 [not-found (request? . -> . response?)]
 [not-allowed (request? . -> . response?)]
 [get-request-header (request? bytes? . -> . (or/c false/c bytes?))]
 [bytes->string (bytes? . -> . (or/c false/c string?))])
