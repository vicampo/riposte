#lang racket/base

(require (file "common.rkt")
         web-server/dispatch)

(define (frack req s)
  (define h (make-header #"Content-Type" (string->bytes/utf-8 s)))
  (response/empty #:code 200
                  #:headers (list h)))

(define-values (start url-generator)
  (dispatch-rules
   [("frack" (string-arg)) #:method "get" frack]
   [else not-found]))

(module+ main
  (run start))
