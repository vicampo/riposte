#lang racket/base

(require racket/match
         web-server/http/request-structs
         web-server/dispatch
         json
         (file "common.rkt"))

(define (login req)
  (match (request-post-data/raw req)
    [#f (response/empty #:code 400)]
    [(? bytes? b)
     (define js (with-handlers ([exn:fail?
                                 (lambda (err)
                                   (log-error "~a" (exn-message err))
                                   (response/empty #:code 401))])
                  (bytes->jsexpr b)))
     (match js
       [(hash-table ('username (? string? u))
                    ('password (? string? p)))
        (define registered-password (hash-ref user-passwords u #f))
        (cond [(and (string? registered-password)
                    (string=? registered-password p)
                    (hash-has-key? user->apikey u))
               (response/jsexpr (hasheq 'apikey (hash-ref user->apikey u))
                                #:code 200)]
              [else
               (response/empty #:code 402)])]
       [else
        (response/empty #:code 403)])]))

(define apikey->user
  (hash "SUPER SECRET" "jesse"))

(define user->apikey
  (make-hash (hash-map apikey->user (lambda (k v) (cons v k)))))

(define user-passwords
  (hash "jesse" "password"))

(define account-details
  (hash "jesse" (hasheq 'created-at 123456
                        'in-arrears #t)))

(define (fetch-account-details username)
  (hash-ref account-details username void))

(define (apikey->username key)
  (hash-ref apikey->user key #f))

(define (request->apikey req)
  (match (headers-assq* #"apikey" (request-headers/raw req))
    [(? header? h)
     (with-handlers ([exn:fail? (lambda (err) #f)])
       (bytes->string/utf-8 (header-value h)))]
    [else #f]))

(define (view-account req)
  (define k (request->apikey req))
  (match (request->apikey req)
    [#f (response/empty #:code 403)]
    [(? string? k)
     (match (apikey->username k)
       [#f (response/empty #:code 403)]
       [(? string? s)
        (match (fetch-account-details s)
          [(? jsexpr? js)
           (response/jsexpr js #:code 200)]
          [else
           (response/empty #:code 500)])])]
    [else
     (response/empty #:code 500)]))

(define-values (start url-generator)
  (dispatch-rules
   [("v1" "login") #:method "post" login]
   [("v1" "account-details") #:method "get" view-account]
   [else not-found]))

(module+ main
  (run start))
