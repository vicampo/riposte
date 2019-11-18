#lang racket

(require (file "common.rkt"))

(define timeouts
  (hash 'disneyland 10
        'mcdonalds 1))

(define (slow req)
  (sleep 60)
  (response/empty))

(define (join req)
  (log-error "body: ~a" (request->jsexpr req))
  (match (request->jsexpr req)
    [(? hash? h)
     (match (hash-ref h 'destination #f)
       [(? string? s)
        (define dest (string-downcase (string-trim s)))
        (define host (string->symbol dest))
        (match (hash-ref timeouts host #f)
          [#f
           (response/empty #:code 404)]
          [(? exact-nonnegative-integer? t)
           (sleep t)
           (define header (make-header #"Location"
                                       (string->bytes/utf-8 dest)))
           (response/jsexpr (hasheq 'ticket_for dest)
                            #:code 200
                            #:headers (list header))]
          [else
           (response/empty #:code 500)])]
       [else
        (response/empty #:code 400)])]
    [else
     (response/jsexpr #:code 400)]))

(define-values (start url-generator)
  (dispatch-rules
   [("sloooooooow") #:method "get" slow]
   [("sloooooooow") #:method (regexp ".*") not-allowed]
   [("join-queue") #:method "post" join]
   [("join-queue") #:method (regexp ".*") not-allowed]))

(module+ main
  (run start))
