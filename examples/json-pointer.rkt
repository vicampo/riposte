#lang racket/base

(require racket/match
         web-server/http/request-structs
         web-server/dispatch
         json
         (file "common.rkt"))

(define (foo req)
  (response/jsexpr
   (hasheq 'items (list "wow!")
           'more (hasheq 'zoom "zow!"))))

(define-values (start url-generator)
  (dispatch-rules
   [("foo") #:method "get" foo]
   [("foo") #:method (regexp ".*") not-allowed]))

(module+ main
  (run start))
