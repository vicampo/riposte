#lang racket/base

(require racket/match
         web-server/http/request-structs
         web-server/dispatch
         json
         (file "common.rkt"))

(define baz 42)

(define bomb -59.8)

(define (robby req)
  (response/jsexpr (hasheq 'foo "wow!"
                           'bar (list "foo")
                           'baz baz
                           'bomb bomb)))

(define (jay req)
  (define body (request->jsexpr req))
  (match (request->jsexpr req)
    [(hash-table ('sum s)
                 ('product p)
                 ('difference d))
     (cond [(and (= s (+ baz bomb))
                 (= p (* baz bomb))
                 (= d (- baz bomb)))
            (response/empty #:code 201
                            #:headers (list (make-header #"Location" #"/jay/zee")))])]
    [else
     (response/empty #:code 400)]))

(define-values (start url-generator)
  (dispatch-rules
   [("robby") #:method "get" robby]
   [("robby") #:method (regexp ".*") not-allowed]
   [("jay") #:method "post" jay]
   [("jay") #:method (regexp ".*") not-allowed]))

(module+ main
  (run start))
