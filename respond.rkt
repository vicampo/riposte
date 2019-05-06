#lang racket/base

(provide respond
         respond/html
         respond/json
         respond/text
         respond/css
	 not-allowed
	 not-found
	 bad-request
	 internal-server-error
         minimal-response-for-code
         set-header
         set-location)

(require (only-in web-server/http
		  response/full)
         (only-in web-server/http/request-structs
		  header?
		  make-header)
         (only-in net/cookies/server
                  cookie?
                  cookie->set-cookie-header)
         (only-in web-server/http/response-structs
                  response
                  response?
                  response-headers)
         (only-in racket/list
                  empty?)
         racket/contract)

;; See https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
;;
;; The list is incomplete; it reflects some common use cases
;; in my own coding. You can extend the list as you see fit.
(define/contract messages
  (hash/c exact-positive-integer? string?)
  (hasheq 200 "OK"
          201 "Created"
          202 "Accepted"
          204 "No Content"

          300 "Multiple Choices"
          301 "Moved Permanently"
          302 "Found"
          303 "See Other"
          307 "Temporary Redirect"
          308 "Permanent Redirect"

          400 "Bad Request"
          401 "Unauthorized"
          403 "Forbidden"
          404 "Not Found"
          405 "Method Not Allowed"
          411 "Length Required"
          415 "Unsupported Media Type"
          422 "Unprocessable Entity"
          423 "Locked"

          500 "Internal Server Error"))

;; Use this as the HTTP response message in case we don't match
;; anything in the list above
(define/contract fallback-message
  string?
  "Unknown")

(define/contract (message-for-code code)
  (exact-positive-integer? . -> . string?)
  (hash-ref messages code fallback-message))

(define/contract (mime thing)
  (any/c . -> . (or/c bytes? false/c))
  (cond ((string? thing)
	 (string->bytes/utf-8 thing))
	((bytes? thing)
	 thing)
	(else
	 #f)))

(define/contract (message m code)
  ((or/c false/c bytes? string?) exact-positive-integer? . -> . bytes?)
  (cond ((eq? m #f)
         (string->bytes/utf-8 (message-for-code code)))
        ((bytes? m)
         m)
        ((string? m)
         (string->bytes/utf-8 m))))

(define/contract (body thing)
  ((or/c false/c
         string?
         bytes?
         (listof (or/c string? bytes?)))
   . -> .
   (listof bytes?))
  (cond ((eq? #f thing)
         (list))
        ((string? thing)
	 (list (string->bytes/utf-8 thing)))
	((bytes? thing)
	 (list thing))
	((list? thing)
	 (map ensure-bytes thing))))

(define/contract (ensure-bytes thing)
  ((or/c bytes?
         string?
         false/c
         boolean?
         number?
         symbol?)
   . -> .
   bytes?)
  (cond ((bytes? thing)
	 thing)
	((string? thing)
	 (string->bytes/utf-8 thing))
        ((eq? thing #f)
         #"0")
        ((eq? thing #t)
         #"1")
        ((number? thing)
         (string->bytes/utf-8 (format "~a" thing)))
        ((symbol? thing)
         (string->bytes/utf-8 (format "~a" thing)))))

(define/contract (might-be-header-key? x)
  (any/c . -> . boolean?)
  (or (string? x)
      (symbol? x)))

(define/contract (might-be-header-value? x)
  (any/c . -> . boolean?)
  (or (string? x)
      (symbol? x)))

(define/contract (might-be-header? x)
  (any/c . -> . boolean?)
  (or (header? x)
      (and (pair? x)
           (might-be-header-key? (car x))
           (might-be-header-value? (cdr x)))))

(define/contract (ensure-header x)
  ((or/c might-be-header?
         cookie?)
   . -> .
   header?)
  (cond ((header? x) x)
	((pair? x)
	 (make-header (ensure-bytes (car x))
                      (ensure-bytes (cdr x))))
        ((cookie? x)
         (make-header #"Set-Cookie"
                      (cookie->set-cookie-header x)))))

(define/contract (headers thing)
  (list? . -> . (listof header?))
  (map ensure-header thing))

;; The main function: convenience wrapper around response/full
(define/contract
  (respond #:code [code/kw 200]
           #:message [message/kw #f]
           #:seconds [seconds/kw (current-seconds)]
           #:mime    [mime/kw #f]
           #:headers [headers/kw (list)]
           #:cookies [cookies/kw (list)]
           #:body    [body/kw #f])
  (->* ()
       (#:code exact-positive-integer?
        #:message (or/c false/c
                        bytes?
                        string?)
        #:seconds integer?
        #:mime (or/c false/c
                     bytes?
                     string?)
        #:headers (listof might-be-header?)
        #:cookies (listof cookie?)
        #:body (or/c false/c
                     bytes?
                     string?
                     (listof (or/c bytes?
                                   string?))))
       response?)
  (response/full
   code/kw
   (message message/kw code/kw)
   seconds/kw
   (mime mime/kw)
   (headers (append headers/kw cookies/kw))
   (body body/kw)))

(define/contract (minimal-response-for-code code)
  (exact-positive-integer? . -> . response?)
  (respond #:code code
           #:message (message-for-code code)))

(define (not-allowed req . whatever)
  (minimal-response-for-code 405))

(define (not-found req . whatever)
  (minimal-response-for-code 404))

(define (bad-request . whatever)
  (minimal-response-for-code 400))

(define (internal-server-error . whatever)
  (minimal-response-for-code 500))

;; Some commonly used media types
(define (respond/html #:code [code/kw 200]
                      #:message [message/kw #f]
                      #:seconds [seconds/kw (current-seconds)]
                      #:headers [headers/kw (list)]
                      #:cookies [cookies/kw (list)]
                      #:body    [body/kw #f])
  (respond #:code code/kw
           #:message message/kw
           #:seconds seconds/kw
           #:mime #"text/html;charset=utf-8"
           #:headers headers/kw
           #:cookies cookies/kw
           #:body body/kw))

(define (respond/json #:code [code/kw 200]
                      #:message [message/kw #f]
                      #:seconds [seconds/kw (current-seconds)]
                      #:headers [headers/kw (list)]
                      #:cookies [cookies/kw (list)]
                      #:body    [body/kw #f])
  (respond #:code code/kw
           #:message message/kw
           #:seconds seconds/kw
           #:mime #"application/json;charset=utf-8"
           #:headers headers/kw
           #:cookies cookies/kw
           #:body body/kw))

(define (respond/css #:code [code/kw 200]
                     #:message [message/kw #f]
                     #:seconds [seconds/kw (current-seconds)]
                     #:headers [headers/kw (list)]
                     #:cookies [cookies/kw (list)]
                     #:body    [body/kw #f])
  (respond #:code code/kw
           #:message message/kw
           #:seconds seconds/kw
           #:mime #"text/css;charset=utf-8"
           #:headers headers/kw
           #:cookies cookies/kw
           #:body body/kw))

(define (respond/text #:code [code/kw 200]
                      #:message [message/kw #f]
                      #:seconds [seconds/kw (current-seconds)]
                      #:headers [headers/kw (list)]
                      #:cookies [cookies/kw (list)]
                      #:body    [body/kw #f])
  (respond #:code code/kw
           #:message message/kw
           #:seconds seconds/kw
           #:mime #"text/plain;charset=utf-8"
           #:headers headers/kw
           #:cookies cookies/kw
           #:body body/kw))

(define/contract (set-header resp head val)
  (response? might-be-header-key? might-be-header-value? . -> . response?)
  (define h (ensure-header (cons head val)))
  (struct-copy response
               resp
               [headers (cons h (response-headers resp))]))

(define/contract (set-location resp loc)
  (response? might-be-header-value? . -> . response?)
  (set-header resp 'Location loc))
