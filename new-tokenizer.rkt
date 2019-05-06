#lang racket/base

(provide tokenize)

(require racket/contract
         racket/match
         racket/format
         (only-in racket/list
                  take
                  drop
                  empty?)
         br-parser-tools/lex
         (only-in net/url
                  string->url)
         (only-in misc1/syntax
                  with-input-string
                  with-input-bytes)
         (only-in (file "util.rkt")
                  port->chars))

(module+ test
  (require rackunit))

#|

Identifiers: $ followed by a sequence of letters, numbers, and '_'

|#

(define expecting-uri? #f)
(define parsed-base-url-parameter? #f)

(define/contract (char-length c)
  ((or/c char? (listof (or/c char? eof-object?))) . -> . exact-nonnegative-integer?)
  (match c
    [(? char?)
     (char-utf-8-length c)]
    [(list)
     0]
    [(cons (? char? c) more)
     (+ (char-utf-8-length c)
        (char-length more))]
    [(cons (? eof-object? x) more)
     0]))

(define/contract (peek-chars num-to-peek)
  (exact-integer? . -> . (listof (or/c char? eof-object?)))
  (define (peek-me num-to-peek already-peeked)
    (cond [(<= num-to-peek 0)
           already-peeked]
          [else
           (peek-me (sub1 num-to-peek)
                    (cons (peek-char (current-input-port) (char-length already-peeked))
                          already-peeked))]))
  (reverse (peek-me num-to-peek (list))))

(define/contract (import-filename)
  (-> (or/c false/c position-token?))
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-non-whitespace-chars)
    (match (peek-char)
      [(or (? char-whitespace?)
           (? eof-object?))
       (list)]
      [(? char?)
       (cons (read-char) (read-non-whitespace-chars))]))
  (define filename (read-non-whitespace-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (match filename
    [(list)
     #f]
    [else
     (position-token (list->string filename)
                     (position p1 l1 c1)
                     (position p2 l2 c2))]))

(define/contract (after-import)
  (-> (or/c false/c position-token?))
  (define c (peek-char))
  (match c
    [(? eof-object?)
     (read-char)
     #f]
    [(? char-whitespace?)
     (read-char)
     (after-import)]
    [else
     (import-filename)]))

(define/contract (import)
  (-> (listof position-token?))
  (define peeked (peek-chars (string-length "import")))
  (match (peek-chars (string-length "import"))
    [(list #\i #\m #\p #\o #\r #\t)
     (define-values (l1 c1 p1)
       (port-next-location (current-input-port)))
     (for ([c (string->list "import")])
       (read-char))
     (define-values (l2 c2 p2)
       (port-next-location (current-input-port)))
     (define toke (position-token 'import
                                  (position p1 l1 c1)
                                  (position p2 l2 c2)))
     (match (after-import)
       [#f
        (list toke)]
       [(? position-token? pt)
        (list toke pt)])]))

(define/contract (identifier)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-identifier-chars)
    (match (peek-char)
      [(or (? char-whitespace?)
           (? eof-object?))
       (list)]
      [(? char-alphabetic?)
       (cons (read-char) (read-identifier-chars))]
      [(? char? c)
       (error (format "Unexpected non-alphabetic character \"~a\" encountered while reading an identifier (line ~a column ~a)."
                      c
                      l1
                      c1))]))
  (define first-char (read-char))
  (unless (eq? first-char #\$)
    (error (format "Expected a dollar sign ($) to begin an identifier; found ~a instead." first-char)))
  (define ident (read-identifier-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (match ident
    [(list)
     (error "Bare dollar sign ($) encountered.")]
    [else
     (position-token (cons 'identifier (list->string ident))
                     (position p1 l1 c1)
                     (position p2 l2 c2))]))

(define/contract (comment)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-comment-chars)
    (match (read-char)
      [(or (? eof-object?) #\newline)
       (list)]
      [(? char? c)
       (cons c (read-comment-chars))]))
  (define first (read-char))
  (unless (eq? #\# first)
    (error (format "Unexpected first character (~a) of a comment." first)))
  (define comment (read-comment-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (cons 'comment (list->string comment))
                  (position p1 l1 c2)
                  (position p2 l2 c2)))

(define/contract (uri-template)
  (-> position-token?)
  (log-error "uri-template...")
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-url-chars)
    (match (read-char)
      [(? eof-object?)
       (list)]
      [(? char-whitespace?)
       (list)]
      [(? char? c)
       (cons c (read-url-chars))]))
  (define chars (read-url-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (define template (list->string chars))
  (log-error "template = ~a" template)
  (position-token (cons 'uri-template template)
                  (position p1 l1 c2)
                  (position p2 l2 c2)))

(define/contract (json-pointer)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-json-pointer-chars)
    (match (read-char)
      [(? eof-object?)
       (list)]
      [(? char-whitespace?)
       (list)]
      [(? char? c)
       (cons c (read-json-pointer-chars))]))
  (define chars (read-json-pointer-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (cons 'json-pointer (list->string chars))
                  (position p1 l1 c2)
                  (position p2 l2 c2)))

(define/contract (json-string)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-string-chars)
    (match (read-char)
      [(? eof-object?)
       (error (format "Unexpected end-of-file while reading a JSON string starting at line ~a and column ~a"
                      l1
                      c1))]
      [#\"
       (list)]
      [(? char? c)
       (cons c (read-string-chars))]))
  (define first (read-char))
  (unless (char? first)
    (error (format "Unexpected enf-of-file or malformed character found while reading a JSON string literal starting at line ~a and column ~a."
                   l1
                   c1)))
  (unless (and (char? first)
               (char=? first #\"))
    (error (format "Unexpected initial character (~a) found while starting to read a JSON string literal at line ~a and column ~a."
                   l1
                   c1)))
  (define chars (read-string-chars))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (cons 'json-string (list->string chars))
                  (position p1 l1 c1)
                  (position p2 l2 c2)))

(define/contract (number)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-integer-chars)
    (match (peek-char)
      [(or (? eof-object?)
           (? char-whitespace?)
           #\.)
       (list)]
      [(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (cons (read-char) (read-integer-chars))]
      [(? char? c)
       (list)]))
  (define chars (read-integer-chars))
  (when (empty? chars)
    (error "Failed to read any integer characters!"))
  (match (peek-char)
    [#\.
     (read-char)
     (define after-decimal-digits (read-integer-chars))
     (when (empty? after-decimal-digits)
       (error "Read a number, then a \".\", but then failed to read more digits."))
     (define-values (l2 c2 p2)
       (port-next-location (current-input-port)))
     (define n (format "~a.~a"
                       (list->string chars)
                       (list->string after-decimal-digits)))
     (parameterize ([read-decimal-as-inexact #f])
       (position-token (string->number n)
                       (position p1 l1 c1)
                       (position p2 l2 c2)))]
    [else
     (define-values (l2 c2 p2)
       (port-next-location (current-input-port)))
     (position-token (string->number (list->string chars))
                     (position p1 l1 c1)
                     (position p2 l2 c2))]))

#;
(define/contract (eat-whitespace)
  (-> void)
  (define c (peek-char))
  (when (and (char? c)
             (char-whitespace? c))
    (read-char)
    (eat-whitespace)))

(define/contract (eat-whitespace chars start)
  ((listof char?) position? . -> . (values (listof char?) position?))
  (define (consume chars)
    (match chars
    [(list)
     (list)]
    [(cons (? char-whitespace? c) _)
     (cons c (consume (cdr chars)))]
    [else
     chars]))
  (define consumed (consume chars))
  (values (drop chars (length consumed))
          (add-position start consumed)))

#|

Three kinds of commands:

METHOD URI-TEMPLATE [ more stuff ]

METHOD $identifier URI-TEMPLATE [ more stuff ]

METHOD null URI-TEMPLATE [ more stuff ]

METHOD true URI-TEMPLATE [ more stuff ]

METHOD false URI-TEMPLATE [ more stuff ]

METHOD "string" URI-TEMPLATE [ more stuff ]


|#

(define/contract (lex-jsonish-stuff)
  (-> (listof position-token?))
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define c (peek-char))
  (match c
    [(? eof-object?)
     (read-char)
     (list)]
    [(? char-whitespace?)
     (read-char)
     (lex-jsonish-stuff)]
    [(or #\{ #\} #\[ #\])
     (read-char)
     (define-values (l2 c2 p2)
       (port-next-location (current-input-port)))
     (cons (position-token (string->symbol (~a c))
                           (position p1 l1 c1)
                           (position p2 l2 c2))
           (lex-jsonish-stuff))]
    [#\"
     (cons (json-string)
           (lex-jsonish-stuff))]
    [else
     (match (peek-chars 6)
       [(list #\n #\u #\l #\l (or (? eof-object?)
                                  (? char-whitespace?)
                                  #\,
                                  #\}
                                  #\]) _)
        (for ([c (string->list "null")])
          (read-char))
        (define-values (l2 c2 p2)
          (port-next-location (current-input-port)))
        (cons (position-token 'json-null
                              (position p1 l1 c1)
                              (position p2 l2 c2))
              (lex-jsonish-stuff))]
       [(list #\t #\r #\u #\e (or (? eof-object?)
                                  (? char-whitespace?)
                                  #\,
                                  #\}
                                  #\]) _)
        (for ([c (string->list "true")])
          (read-char))
        (define-values (l2 c2 p2)
          (port-next-location (current-input-port)))
        (cons (position-token 'json-true
                              (position p1 l1 c1)
                              (position p2 l2 c2))
              (lex-jsonish-stuff))]
       [(list #\f #\a #\l #\s #\e (or (? eof-object?)
                                  (? char-whitespace?)
                                  #\,
                                  #\}
                                  #\]))
        (for ([c (string->list "false")])
          (read-char))
        (define-values (l2 c2 p2)
          (port-next-location (current-input-port)))
        (cons (position-token 'json-false
                              (position p1 l1 c1)
                              (position p2 l2 c2))
              (lex-jsonish-stuff))]
       [else
        (list)])]))

(define/contract (peek-until-eof-or-non-whitespace-char num-already-peeked)
  (exact-nonnegative-integer? . -> . (or/c eof-object? (and/c char? (not/c char-whitespace?))))
  (match (peek-char (current-input-port) (* 8 num-already-peeked))
    [(? eof-object?)
     eof]
    [(? char-whitespace?)
     (peek-until-eof-or-non-whitespace-char (add1 num-already-peeked))]
    [(? char? c)
     c]))

; we just peeked a { character; is this followed by a "
; character (possibly preceded by some whitespace)? If so,
; it is a JSON object
(define/contract (looking-at-json-object?)
  (-> boolean?)
  ; we need to do an unbounded amount of peeking here, looking for either
  ; EOF or a character
  (match (peek-until-eof-or-non-whitespace-char 1)
    [(? eof-object?)
     #f]
    [#\"
     #t]
    [else
     #f]))

(define/contract (http-method)
  (-> (listof position-token?))
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define (read-http-method-chars)
    (match (peek-char)
      [(or (? eof-object?)
           (? char-whitespace?))
       (list)]
      [(? char-upper-case?)
       (cons (read-char) (read-http-method-chars))]
      [(? char? c)
       (error (format "Unexpected non-uppercase character \"~a\" encountered while reading an HTTP method."
                      c))]))
  (define chars (read-http-method-chars))
  (when (empty? chars)
    (error "Failed to read any HTTP characters!"))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (define method/token (position-token (cons 'http-method (list->string chars))
                                       (position p1 l1 c1)
                                       (position p2 l2 c2)))
  (match (peek-char)
    [(? eof-object?)
     (list method/token)]
    [#\$
     (define id/token (identifier))
     (define to/token (to))
     (define template/token (uri-template))
     (list method/token id/token to/token template/token)]
    [#\{
     (cond [(looking-at-json-object?)
            (define json-stuff (lex-jsonish-stuff))
            (define to/token (to))
            (define template/token (uri-template))
            (append (list method/token)
                    json-stuff
                    (list to/token template/token))]
           [else
            (list method/token (uri-template))])]
    #;[(or #\t #\f #\n #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)]
    [(or #\[ #\")
     (define json-stuff (lex-jsonish-stuff))
     (define to/token (to))
     (define template/token (uri-template))
     (append (list method/token)
             json-stuff
             (list to/token template/token))]
    [else
     ;; interpret what follows as a URI Template
     (list method/token (uri-template))]))

(module+ test
  #;
  (with-input-string "POST $foo to bar"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token '(identifier . "foo") (position 6 1 5) (position 10 1 9))
                   (position-token 'to (position 11 1 10) (position 13 1 12))
                   (position-token
                    '(uri-template . "bar")
                    (position 14 1 16)
                    (position 17 1 16)))))
  #;
  (with-input-string "GET bar"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list
                   (position-token '(http-method . "GET") (position 1 1 0) (position 4 1 3))
                   (position-token '(uri-template . "bar") (position 5 1 7) (position 8 1 7)))))
  #;
  (let ([program "POST { \"hi\": \"there\" } to whatever"])
    (log-error "~a" program)
    (with-input-string program
      (port-count-lines! (current-input-port))
      (check-equal? (http-method)
                    (list))))
  #;
  (with-input-string "POST [ 1, 2, 3 ] to whatever"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list)))
  #;
  (with-input-string "POST null to whatever"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list)))
  #;
  (with-input-string "POST false to whatever"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list)))
  #;
  (with-input-string "POST true to whatever"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list)))
  #;
  (with-input-string "POST \"hi\" to whatever"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list)))
  #;
  (with-input-string "GET null"
    (port-count-lines! (current-input-port))
    (check-equal? (http-method)
                  (list))))

(define/contract (responds:responds chars start))

(define/contract (responds:with chars start))

(define/contract (responds:status-code chars start))

(define/contract (responds chars start)
  ((listof char?) position? -> position-token?)
  (match chars
    [(list #\r #\e #\s #\p #\o #\n #\d #\s)
     (define pos-after-responds
       (add-position start (take chars (string-length "responds"))))
     (define chars-after-responds (drop chars (string-length "responds")))
     (position-token 'responds
                     start
                     pos-after-responds)]
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s (? char-whitespace?) _)
     (define pos-after-responds
       (add-position start (take chars (string-length "responds"))))
     (define chars-after-responds (drop chars (string-length "responds")))
     (define responds-token
       (position-token 'responds
                       start
                       pos-after-responds))
     (define first-whitespace-result
       (eat-whitespace chars-after-responds pos-after-responds))
     (define pos-after-first-whitespace
       (lexer-state-end-position first-whitespace-result))
     (define chars-after-first-whitespace
       (lexer-state-characters first-whitespace-result))
     (define pos-after-with (add-position pos-after-first-whitespace
                                          (string->list "with")))
     (define chars-after-with (drop chars-after-first-whitespace (string-length "with")))
     (define with-token
       (position-token 'with
                       pos-after-first-whitespace
                       pos-after-with))
     (define second-whitespace-result
       (eat-whitespace chars-after-with pos-after-with))
     (define pos-after-second-whitespace
       (lexer-state-end-position second-whitespace-result))
     (define chars-after-second-whitespace
       (lexer-state-characters second-whitespace-result))
     (define status-code-token (position-token (cons 'http-status-code
                                                     (list->string
                                                      (take chars-after-second-whitespace 3)))
                                               pos-after-second-whitespace
                                               (add-position pos-after-second-whitespace
                                                             (take chars-after-second-whitespace 3))))
     (list responds-token
           with-token
           status-code-token)]))

(define/contract (with)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define keyword "with")
  (define keyword-read
    (for/list ([c (string->list keyword)])
      (read-char)))
  (define keyword-read/string (list->string (filter char? keyword-read)))
  (unless (string=? keyword keyword-read/string)
    (error (format "While tokenizing \"~a\" at line ~a and column ~a, we got something unexpected: ~a"
                   keyword
                   l1
                   c1
                   keyword-read/string)))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (string->symbol keyword)
                  (position p1 l1 c1)
                  (position p2 l2 c2)))

(define/contract (http-status-code)
  (-> position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define first-char
    (match (peek-char)
      [(? eof-object?)
       (error "End-of-file encountered while lexing an HTTP status code.")]
      [(? char-whitespace?)
       (error (format "Whitespace found at line ~a and column ~a while lexing an HTTP status code."
                      l1
                      c1))]
      [(or #\2 #\3 #\4 #\5)
       (read-char)]
      [(? char? c)
       (error (format "Unexpected character (~a) found at line ~a and column ~a while lexing an HTTP status code."
                      c
                      l1
                      c1))]))
  (define second-char
    (match (peek-char)
      [(? eof-object?)
       (error "End-of-file encountered while lexing an HTTP status code.")]
      [(? char-whitespace?)
       (error (format "Whitespace found at line ~a and column ~a while lexing an HTTP status code."
                      l1
                      c1))]
      [(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
       (read-char)]
      [(? char? c)
       (error (format "Unexpected character (~a) found at line ~a and column ~a while lexing an HTTP status code."
                      c
                      l1
                      c1))]))
  (define third-char
    (match (peek-char)
      [(? eof-object?)
       (error "End-of-file encountered while lexing an HTTP status code.")]
      [(? char-whitespace?)
       (error (format "Whitespace found at line ~a and column ~a while lexing an HTTP status code."
                      l1
                      c1))]
      [(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
       (read-char)]
      [(? char? c)
       (error (format "Unexpected character (~a) found at line ~a and column ~a while lexing an HTTP status code."
                      c
                      l1
                      c1))]))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (cons 'http-status-code (list->string (list first-char second-char third-char)))
                  (position c1 l1 c1)
                  (position c2 l2 c2)))

(define/contract (consume-keyword keyword)
  (string? . -> . position-token?)
  (define-values (l1 c1 p1)
    (port-next-location (current-input-port)))
  (define keyword-read
    (for/list ([c (string->list keyword)])
      (read-char)))
  (define keyword-read/string (list->string (filter char? keyword-read)))
  (unless (string=? keyword keyword-read/string)
    (error (format "While tokenizing \"~a\" at line ~a and column ~a, we got something unexpected: ~a"
                   keyword
                   l1
                   c1
                   keyword-read/string)))
  (define-values (l2 c2 p2)
    (port-next-location (current-input-port)))
  (position-token (string->symbol keyword)
                  (position p1 l1 c1)
                  (position p2 l2 c2)))

(define/contract (exists)
  (-> position-token?)
  (consume-keyword "exists"))

(define/contract (to)
  (-> position-token?)
  (consume-keyword "to"))

(define/contract (add-position initial char-or-chars)
  (position? (or/c char? (listof char?)) . -> . position?)
  (match char-or-chars
    [(list)
     initial]
    [#\newline
     (position (add1 (position-offset initial))
               (add1 (position-line initial))
               1)]
    [(cons #\newline _)
     (add-position (position (add1 (position-offset initial))
                             (add1 (position-line initial))
                             1)
                   (cdr char-or-chars))]
    [(? char?)
     (position (add1 (position-offset initial))
               (add1 (position-line initial))
               (add1 (position-col initial)))]
    [(cons (? char?) _)
     (add-position (position (add1 (position-offset initial))
                             (add1 (position-line initial))
                             (add1 (position-col initial)))
                   (cdr char-or-chars))]))

(define-struct/contract lexer-result
  ([end-position position?]
   [tokens (listof position-token?)]
   [characters (listof char?)]))

(define/contract (assignment chars start)
  ((listof char?) position? . -> . position-token?)
  (match chars
    [(list-rest #\: #\= _)
     (position-token (string->symbol ":=")
                     start
                     (add-position start (take chars 2)))]
    [(list)
     (error (format "Unexpected end-of-file encountered while lexing an assignment (line ~a column ~a)"
                    (position-line start)
                    (position-col start)))]
    [(cons c _)
     (error (format "Unexpected character (~a) encountered while lexing an assignment (line ~a column ~a)"
                    c
                    (position-line start)
                    (position-col start)))]))

(define/contract (single-character chars start)
  ((cons/c (one-of/c #\: #\, #\{ #\} #\[ #\]) (listof char?)) position? . -> . position-token?)
  (position-token (string->symbol (~a (car chars)))
                  start
                  (add-position start (car chars))))

(define/contract (initial chars start)
  ((listof char?) position? . -> . (listof position-token?))
  (match chars
    [(cons (? eof-object?) _)
     (list)]
    [(cons (? char-whitespace? c) _)
     (initial (cdr chars)
              (add-position initial c))]
    [(list-rest #\i #\m #\p #\o #\r #\t more)
     (define result (import chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\$ more)
     (define result (identifier chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\# more)
     (define result (comment chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\/ more)
     (define result (json-pointer chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) more)
     (define result (number chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\: #\= _)
     (define result (assignment chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (? char-upper-case?) _)
     (define result (http-method chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\" more)
     (define result (json-string chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\: #\, #\{ #\} #\[ #\]) _)
     (define result (single-character chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s
                ((? char-whitespace?) ...)
                #\w #\i #\t #\h ((? char-whitespace?) ...)
                (or #\2 #\3 #\4 #\5)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                _)
     (define responds-token (responds))
     (define with-token (with))
     (define status-code-token (http-status-code))
     (append (list responds-token
                   with-token
                   status-code-token)
             (initial))]
    [(list-rest #\e #\x #\i #\s #\t #\s more)
     (cons (exists)
           (initial))]
    [(cons (? char? c) more)
     (error (format "Unexpected character (~a) encountered at line ~a column ~a. Bailing out."
                    c
                    (position-line start)
                    (position-col start)))]))

(define/contract (tokenize in [start (position 0 1 1)])
  (->* ((or/c string? bytes? input-port? path? (listof char?)))
       (position?)
       (listof token?))
  (cond [(string? in)
         (tokenize (string->list in) start)]
        [(bytes? in)
         (tokenize (bytes->string/utf-8 in) start)]
        [(path? in)
         (define (toke-it)
           (tokenize (port->chars (current-input-port)) start))
         (call-with-input-file in toke-it)]
        [(input-port? in)
         (tokenize (port->chars in) start)]
        [(list? in)
         (initial in start)]))

(module+ test
  (let ([program #<<RIPOSTE
import foo.rip
$a := 4.567
RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list
                   (position-token 'import (position 1 1 0) (position 7 1 6))
                   (position-token "foo.rip" (position 8 1 7) (position 15 1 14))
                   (position-token '(identifier . "a") (position 16 2 0) (position 18 2 2))
                   (position-token ':= (position 19 2 3) (position 21 2 5))
                   (position-token 4567/1000 (position 22 2 6) (position 27 2 11))))))

#;
(module+ test
  (let ([program #<<RIPOSTE
import base.rip

# Test if the v1/search endpoint provides the same price/qty_increments as v1/catalog/products

## v1/search
$searchQueryParams := {
    "pagination_page": "1",
    "pagination_limit": "1",
    "in_stock_vendors": "VICAMPO",
    "sort": "relevance",
    "fields": "ratings,expert_rating,type,product_id,campaign_id,name,manufacturer_name,tags,price,stocks,images,legal,customer_rating,customer_rating_count",
    "embed": "bundle_items,has_been_bought,expert_reviews"
}

GET search{?searchQueryParams*} responds with 2xx

RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

#;
(module+ test
  (let ([program #<<RIPOSTE
import base.rip
import token-auth.rip

$invalidData := {}

POST $invalidData customers/addresses responds with 422

$validData := {
    "first_name": "BLA",
    "last_name": "BLO",
    "city": "TestCity",
    "country": "de",
    "is_default_billing_address": 1,
    "is_default_shipping_address": 1,
    "street_name": "teststreet",
    "street_number": 33,
    "zip": 32145
}

POST $validData customers/addresses responds with 201

/customer_address_id exists
RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

(module+ main

  (require racket/cmdline)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (tokenize file-to-process))
