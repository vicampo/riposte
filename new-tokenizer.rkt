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

(define-struct/contract lexer-result
  ([end-position position?]
   [tokens (listof position-token?)]
   [characters (listof char?)]))

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

(define/contract (import-filename chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define (read-non-whitespace-chars read-so-far remaining)
    (match remaining
      [(list)
       read-so-far]
      [(cons (? char-whitespace?) _)
       read-so-far]
      [(cons (? char? c) _)
       (read-non-whitespace-chars (cons c read-so-far) (cdr remaining))]))
  (define filename-chars (read-non-whitespace-chars (list) chars))
  (define end-position (add-position start filename-chars))
  (define token (position-token (cons 'filename (list->string (reverse filename-chars)))
                                start
                                end-position))
  (lexer-result end-position
                (list token)
                (drop chars (length filename-chars))))

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

(define/contract (import chars start)
  ((listof char?) position? . -> . lexer-result?)
  (log-error "import: ~a" chars)
  (match chars
    [(list #\i #\m #\p #\o #\r #\t)
     (define end-position (add-position start (string->list "import")))
     (lexer-result end-position
                   (list (position-token 'import)
                         start
                         end-position)
                   (list))]
    [(list-rest #\i #\m #\p #\o #\r #\t (? char-whitespace?) more)
     (define end-position (add-position start (string->list "import")))
     (define import/token (position-token 'import
                                          start
                                          end-position))
     (define chars-of-keyword (take chars (string-length "import")))
     (define remaining-chars (drop chars (string-length "import")))
     (define-values (chars-after-whitespace pos-after-whitespate)
       (eat-whitespace (drop chars (string-length "import"))
                       end-position))
     (define filename/result (import-filename chars-after-whitespace
                                              pos-after-whitespate))
     (lexer-result (lexer-result-end-position filename/result)
                   (cons import/token
                         (lexer-result-tokens filename/result))
                   (lexer-result-characters filename/result))]))

(define/contract (identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define (read-identifier-chars cs)
    (match cs
      [(list)
       (list)]
      [(cons (? char-whitespace?) _)
       (list)]
      [(cons (? char-alphabetic? c) _)
       (cons c (read-identifier-chars (cdr cs)))]
      [(cons (? char? c) _)
       (error (format "Unexpected non-alphabetic character \"~a\" encountered while reading an identifier starting at line ~a column ~a)."
                      c
                      (position-line start)
                      (position-col start)))]))
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons #\$ cs)
     (define ident-chars (read-identifier-chars cs))
     (log-error "read ~a identifier chars: ~a" (length ident-chars) ident-chars)
     (define token-content (cons 'identifier (list->string ident-chars)))
     (define end-position (add-position start (cons #\$ ident-chars)))
     (define token (position-token token-content
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
                   (drop chars (add1 (length ident-chars))))]
    [(cons c _)
     (error (format "Unexpected character (~a) encountered while lexing an identifier at line ~a column ~a."
                    c
                    (position-line start)
                    (position-col start)))]))

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

(define/contract (read-uri-template-text cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons #\{ _)
     (list)]
    [(cons c _)
     (cons c (read-uri-template-text (cdr cs)))]))

(define/contract (uri-template:text chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define text-chars (read-uri-template-text chars))
  (define end-position (add-position start text-chars))
  (define token (position-token (cons 'uri-template-text (list->string text-chars))
                                start
                                end-position))
  (lexer-result end-position
                (list token)
                (drop chars (length text-chars))))

(define/contract (uri-template:template chars start)
  ((listof char?) position? . -> . lexer-result?)
  (error "not implemented yet"))

(define/contract (uri-template chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons #\{ _)
     (define template/result (uri-template:template chars start))
     (define remainder/result (uri-template (lexer-result-characters template/result)
                                            (lexer-result-end-position template/result)))
     (lexer-result (lexer-result-end-position remainder/result)
                   (append (lexer-result-tokens template/result)
                           (lexer-result-tokens remainder/result))
                   (lexer-result-characters remainder/result))]
    [(cons _ _)
     (define text/result (uri-template:text chars start))
     (define remainder/result (uri-template (lexer-result-characters text/result)
                                            (lexer-result-end-position text/result)))
     (lexer-result (lexer-result-end-position remainder/result)
                   (append (lexer-result-tokens text/result)
                           (lexer-result-tokens remainder/result))
                   (lexer-result-characters remainder/result))]))

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

(define/contract (number chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define (read-integer-chars cs)
    (match cs
      [(list)
       (list)]
      [(cons (or (? char-whitespace?) #\.) _)
       (list)]
      [(cons (and (? char? c)
                  (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
             _)
       (cons c (read-integer-chars (cdr cs)))]
      [(cons (? char? c) _)
       (list)]))
  (define digits (read-integer-chars chars))
  (when (empty? chars)
    (error "Failed to read any integer characters!"))
  (define remaining-chars (drop chars (length digits)))
  (define end-position (add-position start digits))
  (match remaining-chars
    [(list)
     (lexer-result end-position
                   (list (position-token (cons 'number (string->number digits))
                                         start
                                         end-position))
                   (list))]
    [(cons #\. _)
     (define after-decimal-digits (read-integer-chars (cdr remaining-chars)))
     (when (empty? after-decimal-digits)
       (error "Read some digits, then a \".\", but then failed to read more digits."))
     (define end-pos-after-dot (add-position end-position (cons #\. after-decimal-digits)))
     (define n (format "~a.~a"
                       (list->string digits)
                       (list->string after-decimal-digits)))
     (parameterize ([read-decimal-as-inexact #f])
       (define token (position-token (cons 'number (string->number n))
                                     start
                                     end-pos-after-dot))
       (lexer-result end-pos-after-dot
                     (list token)
                     (drop chars (length (append digits (list #\.) after-decimal-digits)))))]
    [else
     (define token (position-token (cons 'number (string->number (list->string chars)))
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
                   (drop chars (length digits)))]))

#;
(define/contract (eat-whitespace)
  (-> void)
  (define c (peek-char))
  (when (and (char? c)
             (char-whitespace? c))
    (read-char)
    (eat-whitespace)))

(define/contract (eat-whitespace chars start)
  ((listof char?) position? . -> . lexer-result?)
  (log-error "eat-whitespace: ~a" chars)
  (define (consume cs)
    (log-error "consume: ~a" cs)
    (match cs
      [(list)
       (list)]
      [(cons (? char-whitespace? c) _)
       (cons c (consume (cdr cs)))]
      [else
       (list)]))
  (define consumed (consume chars))
  (log-error "consumed ~a whitespace chars" (length consumed))
  (log-error "consumed chars: ~a" consumed)
  (define remaining-chars (drop chars (length consumed)))
  (log-error "remaining characters: ~a" remaining-chars)
  (define new-position (add-position start consumed))
  (log-error "new position: ~a" new-position)
  (lexer-result new-position
                (list)
                remaining-chars))

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

(define/contract (read-http-method-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons (? char-upper-case? c) _)
     (cons c (read-http-method-chars (cdr cs)))]
    [(cons (? char? c) _)
     (error (format "Unexpected non-uppercase character \"~a\" encountered while reading an HTTP method."
                    c))]))

(define/contract (http-method:method chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define method-chars (read-http-method-chars chars))
  (when (empty? method-chars)
    (error "Failed to read any HTTP method characters!"))
  (define position-after-method (add-position start method-chars))
  (define method/token (position-token (cons 'http-method (list->string method-chars))
                                       start
                                       position-after-method))
  (define remaining-characters (drop chars (length method-chars)))
  (lexer-result position-after-method
                (list method/token)
                remaining-characters))

(define/contract (after-http-method-payload chars start))

(define/contract (after-http-method chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match (lexer-result-characters method/result)
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (? char-whitespace? c) _)
     (after-http-method (cdr chars)
                        (add-position start c))]
    [(cons (or #\$ #\[ #\") _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                 (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest #\t #\r #\u #\e (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define constant "true")
     (define next-position (add-position start (take chars (string-length constant))))
     (define json-token (position-token 'json-true
                                        start
                                        next-position))
     (define whatever/result (after-http-method-payload (drop chars (string-length constant))
                                                        next-position))
     (lexer-result (lexer-result-end-position whatever/result)
                   (cons json-token
                         (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest #\f #\a #\l #\s #\e (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define constant "false")
     (define next-position (add-position start (take chars (string-length constant))))
     (define json-token (position-token 'json-false
                                        start
                                        next-position))
     (define whatever/result (after-http-method-payload (drop chars (string-length constant))
                                                        next-position))
     (lexer-result (lexer-result-end-position whatever/result)
                   (cons json-token
                         (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest #\n #\u #\l #\l (? char-whitespace?) ..1 #\t #\u (? char-whitespace?) ..1 _)
     (define constant "null")
     (define next-position (add-position start (take chars (string-length constant))))
     (define json-token (position-token 'json-null
                                        start
                                        next-position))
     (define whatever/result (after-http-method-payload (drop chars (string-length constant))
                                                        next-position))
     (lexer-result (lexer-result-end-position whatever/result)
                   (cons json-token
                         (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))]))

(define/contract (http-method chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define method/result (http-method:method chars start))
  (match chars
    [(cons (? char-upper-case?) _)
     (define method-chars (takef chars char-upper-case?))
     (define method-end-position (add-position start method-chars))
     (define after-method-chars (drop chars (length method-chars)))
     (define method/token (position-token (cons 'http-method (list->string method-chars))
                                          start
                                          method-end-position))
     (define after-method/result (after-http-method after-method-chars method-end-position))
     (lexer-result (lexer-result-end-position after-method/result)
                   (cons method/token
                         (lexer-result-tokens after-method/result)
                         (lexer-result-characters after-method/result)))]))

(module+ test
  (let ([result (http-method (string->list "POST $foo to bar") (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token '(identifier . "foo") (position 6 1 5) (position 10 1 9))
                   (position-token 'to (position 11 1 10) (position 13 1 12))
                   (position-token
                    '(uri-template-text . "bar")
                    (position 14 1 13)
                    (position 17 1 16)))))
  #;
  (let ([result (http-method (string->list "GET bar") (position 1 1 0))])
    (check-equal? (lexer-result-tokens)
                  (list)))
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

(define/contract (responds:responds chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define keyword-length (string-length "responds"))
  (define chars-of-keyword (take chars keyword-length))
  (define chars-after-keyword (drop chars keyword-length))
  (define pos-after-keyword (add-position start chars-of-keyword))
  (define token (position-token 'responds
                                start
                                pos-after-keyword))
  (match chars
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s _)
     (lexer-result (add-position start (take chars keyword-length))
                   (list token)
                   (drop chars keyword-length))]))

(define/contract (responds:with chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define keyword-length (string-length "with"))
  (define chars-of-keyword (take chars keyword-length))
  (define chars-after-keyword (drop chars keyword-length))
  (define pos-after-keyword (add-position start chars-of-keyword))
  (define token (position-token 'with
                                start
                                pos-after-keyword))
  (match chars
    [(list-rest #\w #\i #\t #\h _)
     (lexer-result pos-after-keyword
                   (list token)
                   (drop chars keyword-length))]))

(define/contract (responds:status-code chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define keyword-length 3)
  (define chars-of-keyword (take chars keyword-length))
  (define chars-after-keyword (drop chars keyword-length))
  (define pos-after-keyword (add-position start chars-of-keyword))
  (define token-content (list->string (chars-of-keyword)))
  (define token (position-token (cons 'http-status-code token-content)
                                start
                                pos-after-keyword))
  (match chars
    [(list-rest (or #\2 #\3 #\4 #\5)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                _)
     (lexer-result pos-after-keyword
                   (list token)
                   (drop chars keyword-length))]))

(define/contract (responds chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define responds/result (responds:responds chars start))
  (match (lexer-result-characters responds/result)
    [(list)
     responds/result]
    [else
     (define first-whitespace-result (eat-whitespace (lexer-result-characters responds/result)
                                                     (lexer-result-end-position responds/result)))
     (match (lexer-result-characters first-whitespace-result)
       [(list)
        responds/result]
       [else
        (define chars-after-first-whitespace (lexer-result-characters first-whitespace-result))
        (define pos-after-first-whitespace (lexer-result-end-position first-whitespace-result))
        (define with/result (responds:with chars-after-first-whitespace
                                           pos-after-first-whitespace))
        (match (lexer-result-characters with/result)
          [(list)
           (lexer-result (lexer-result-end-position with/result)
                         (append (lexer-result-tokens responds/result)
                                 (lexer-result-tokens with/result))
                         (list))]
          [(cons (? char-whitespace?) _)
           (define second-whitespace-result (eat-whitespace (lexer-result-characters with/result)
                                                            (lexer-result-end-position with/result)))
           (match (lexer-result-characters second-whitespace-result)
             [(list)
              (lexer-result (lexer-result-end-position with/result)
                            (append (lexer-result-tokens responds/result)
                                    (lexer-result-tokens with/result))
                            (list))]
             [else
              (define chars-after-second-whitespace (lexer-result-characters second-whitespace-result))
              (define pos-after-second-whitespace (lexer-result-end-position second-whitespace-result))
              (define status-code/result (responds:status-code chars-after-second-whitespace
                                                               pos-after-second-whitespace))
              (lexer-result (lexer-result-end-position status-code/result)
                            (append (lexer-result-tokens responds/result)
                                    (lexer-result-tokens with/result)
                                    (                                    (lexer-result-tokens status-code/result)))
                            (lexer-result-characters status-code/result))])])])]))

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

(define/contract (to chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\t #\o _)
     (define end-position (add-position start (take chars 2)))
     (lexer-result end-position
                   (list (position-token 'to
                                         start
                                         end-position))
                   (drop chars 2))]))

(define/contract (add-position initial char-or-chars)
  (position? (or/c char? (listof char?)) . -> . position?)
  (match char-or-chars
    [(list)
     initial]
    [#\newline
     (position (add1 (position-offset initial))
               (add1 (position-line initial))
               0)]
    [(list-rest #\linefeed #\newline _)
     (add-position (position (add1 (position-offset initial))
                             (add1 (position-line initial))
                             0)
                   (cddr char-or-chars))]
    [(cons #\newline _)
     (add-position (position (add1 (position-offset initial))
                             (add1 (position-line initial))
                             0)
                   (cdr char-or-chars))]
    [(? char?)
     (position (add1 (position-offset initial))
               (position-line initial)
               (add1 (position-col initial)))]
    [(cons (? char?) _)
     (add-position (position (add1 (position-offset initial))
                             (position-line initial)
                             (add1 (position-col initial)))
                   (cdr char-or-chars))]))

(define/contract (assignment chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\: #\= _)
     (define end-position (add-position start (take chars 2)))

     (lexer-result end-position
                   (list (position-token (string->symbol ":=")
                                         start
                                         end-position))
                   (drop chars 2))]
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
    [(list)
     (list)]
    [(cons (? eof-object?) _)
     (list)]
    [(cons (? char-whitespace? c) _)
     (define-values (remaining-chars end-position)
       (eat-whitespace chars start))
     (initial remaining-chars
              end-position)]
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
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s _)
     (define result (single-character chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\x #\i #\s #\t #\s more)
     (cons (exists)
           (initial))]
    [(cons (? char? c) more)
     (error (format "Unexpected character (~a) encountered at line ~a column ~a. Bailing out."
                    c
                    (position-line start)
                    (position-col start)))]))

(define/contract (tokenize in [start (position 1 1 0)])
  (->* ((or/c string? bytes? input-port? path? (listof char?)))
       (position?)
       (listof position-token?))
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

#;
(module+ test
  (let ([program #<<RIPOSTE
import foo.rip
$a := 4.567
RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list
                   (position-token 'import (position 1 1 0) (position 7 1 6))
                   (position-token '(filename . "foo.rip") (position 8 1 7) (position 15 1 14))
                   (position-token '(identifier . "a") (position 16 2 0) (position 18 2 2))
                   (position-token ':= (position 19 2 3) (position 21 2 5))
                   (position-token '(number . 4567/1000) (position 22 2 6) (position 27 2 11))))))

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
