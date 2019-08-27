#lang racket/base

(require (file "common.rkt")
         racket/match
         web-server/http/request-structs
         web-server/dispatch
         json)

(define (flub req)
  (define code
    (match (request-post-data/raw req)
      [#f 400]
      [(? header? h)
       (match (header-value h)
         [#"jp" 200]
         [#"en" 402]
         [else  400])]))
  (response/empty #:code code))

(define-values (start url-generator)
  (dispatch-rules
   [("api" "flub") #:method "post" flub]
   [("api" "flub") #:method (regexp ".*") not-allowed]
   [else not-found]))

(module+ main
  (run start))
