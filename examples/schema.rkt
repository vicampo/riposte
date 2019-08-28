#lang racket/base

(require (file "common.rkt")
         racket/match
         web-server/http/request-structs
         web-server/dispatch
         json)

(define (flub req)
  (match (request-post-data/raw req)
    [#f (response/empty #:code 400)]
    [(? bytes? bs)
     (with-handlers ([exn:fail? (lambda (e) (response/empty #:code 400))])
       (define js (bytes->jsexpr bs))
       (cond [(hash? js)
              (cond [(and (equal? (hash-ref js 'a 'null) #t)
                          (equal? (hash-ref js 'b 'null) 3))
                     (response/jsexpr (hasheq 'age 59 'weight #f) #:code 200)]
                    [else
                     (response/empty #:code 400)])]
             [else
              (response/empty #:code 400)]))]))

(define-values (start url-generator)
  (dispatch-rules
   [("api" "flub") #:method "post" flub]
   [("api" "flub") #:method (regexp ".*") not-allowed]
   [else not-found]))

(module+ main
  (run start))
