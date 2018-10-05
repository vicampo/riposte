#lang racket/base

(provide make-request)

(struct request
  (method
   uri
   payload
   headers))

(define/contract (make-request/no-payload method
                                          uri
                                          #:headers [headers #f])
  (->* (string? string?)
       (#:headers (or/c false/c (hash/c symbol? string?)))
       request?)
  (request method
           uri
           #f
           headers))

(define/contract (make-request/payload method
                                       uri
                                       payload
                                       #:headers [headers #f])
  (->* (string? string? (or/c ejsexpr?
                              bytes?))
       (#:headers (or/c false/c (hash/c symbol? string?)))
       request?)
  (define p
    (match payload
      [#f
       #""]
      [else
       payload]))
  (request method
           uri
           p
           headers))
