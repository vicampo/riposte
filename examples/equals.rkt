#lang racket/base

(require racket/match
         racket/hash
         (file "common.rkt"))

(define (foo/bar req)
  (response/jsexpr (hash 'hey #f)))

(define (post:foo/bar req)
  (match (cons (request->jsexpr req)
               (get-request-header req #"accept"))
    [(cons (? void?) _)
     (response/empty #:code 400)]
    [(cons (not (? hash?)) _ )
     (response/empty #:code 400)]
    [(cons js #f)
     (response/jsexpr (hash-union (hash 'hey #f) (hash 'body js)))]
    [(cons js (? bytes? h))
     (match (bytes->string h)
       [(? string? s)
        (response/jsexpr (hash-union (hash 'hey #t) (hash 'body js 'content-type s)))]
       [else
        (response/empty #:code 400)])]))

(define-values (start url-generator)
  (dispatch-rules
   [("foo" "bar") #:method "get" foo/bar]
   [("foo" "bar") #:method "post" post:foo/bar]
   [("foo" "bar") #:method (regexp ".*") not-allowed]
   [else not-found]))

(module+ main
  (run start))
