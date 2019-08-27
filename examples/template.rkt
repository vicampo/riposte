#lang racket/base

(require (file "common.rkt")
         racket/match
         racket/set
         web-server/http/request-structs
         web-server/dispatch
         json)

(define (flub req id)
  (define code
    (match id
      [1
       (match (request-post-data/raw req)
         [#f
          400]
         [(? bytes? bs)
          (match (bytes->jsexpr bs)
            [(? hash? h)
             (cond [(set=? (set 'a 'b 'c) (list->set (hash-keys h)))
                    (define a (hash-ref h 'a))
                    (define b (hash-ref h 'b))
                    (define c (hash-ref h 'c))
                    (cond [(and (equal? a 4)
                                (equal? b #t)
                                (equal? c (list)))
                           200]
                          [else
                           400])]
                   [else
                    400])]
            [else
             400])])]
      [else
       400]))
  (response/empty #:code code))

(define-values (start url-generator)
  (dispatch-rules
   [("api" "flub" (integer-arg)) #:method "post" flub]
   [else not-found]))

(module+ main
  (run start))
