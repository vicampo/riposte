#lang racket/base

(require racket/match
         web-server/http/request-structs
         web-server/dispatch
         json
         (file "common.rkt"))

(define users
  (hasheq 'mflatt (hasheq 'name "Matthew"
                          'title "Dr.")))

(define (view-user req username)
  (match (hash-ref users (string->symbol username) #f)
    [#f
     (response/empty #:code 404)]
    [(? hash? js)
     (response/jsexpr js)]
    [else
     (response/empty #:code 500)]))

(define-values (start url-generator)
  (dispatch-rules
   [("users" (string-arg)) #:method "get" view-user]))

(module+ main
  (run start))
