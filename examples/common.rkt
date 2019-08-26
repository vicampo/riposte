#lang racket/base

(provide run)

(require racket/list
         racket/contract
         json
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/http/cookie
         web-server/servlet-env
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

(define (run servlet #:port [port 12345])
  (serve/servlet servlet
                 #:port port
                 #:stateless? #t
                 #:command-line? #t
                 #:servlet-regexp (regexp ".*")))

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
   . ->* . response?)])
