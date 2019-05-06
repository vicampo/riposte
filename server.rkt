#lang racket/base

(require racket/contract
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/dispatch
         web-server/servlet-env
         json
         (file "respond.rkt"))

(define/contract (get:foo req)
  (request? . -> . response?)
  (respond/json #:body (jsexpr->bytes (hasheq 'hey "there"))))

(define-values (start url-generator)
  (dispatch-rules
   [("foo") #:method "get" get:foo]
   [("bar") #:method "post" not-allowed]))

(module+ main
  (serve/servlet
   start
   #:command-line? #t
   #:servlet-regexp #rx""
   #:port 12345))
