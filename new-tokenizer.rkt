#lang racket/base

(provide tokenize)

(require racket/contract
         racket/match
         racket/format
         (only-in racket/list
                  take
                  drop
                  empty?
                  takef
                  dropf
                  index-of)
         (only-in racket/function
                  negate)
         br-parser-tools/lex
         brag/support
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
  ;(log-error "import-filename: ~a" chars)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (? char-whitespace?) _)
     (import-filename (cdr chars)
                      (add-position start (car chars)))]
    [(list (not (? char-whitespace?)) ..1)
     (define new-position (add-position start chars))
     (define import/token (position-token (token 'FILENAME (list->string chars))
                                          start
                                          new-position))
     (lexer-result new-position
                   (list import/token)
                   (list))]
    [(list-rest (not (? char-whitespace?)) ..1 (? char-whitespace?) _)
     (define filename-chars (takef chars (negate char-whitespace?)))
     (define filename (list->string filename-chars))
     (define end-position (add-position start filename-chars))
     (define remaining-chars (drop chars (length filename-chars)))
     (define filename/token (position-token (token 'FILENAME filename)
                                            start
                                            end-position))
     (lexer-result end-position
                   (list filename/token)
                   remaining-chars)]))

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
  ;(log-error "import: ~a" chars)
  (match chars
    [(list #\i #\m #\p #\o #\r #\t)
     (define end-position (add-position start (string->list "import")))
     (lexer-result end-position
                   (list (position-token (token 'import))
                         start
                         end-position)
                   (list))]
    [(list-rest #\i #\m #\p #\o #\r #\t (? char-whitespace?) more)
     (define end-position (add-position start (string->list "import")))
     (define import/token (position-token (token 'import)
                                          start
                                          end-position))
     (define chars-of-keyword (take chars (string-length "import")))
     (define remaining-chars (drop chars (string-length "import")))
     (define filename/result (import-filename remaining-chars
                                              end-position))
     (lexer-result (lexer-result-end-position filename/result)
                   (cons import/token
                         (lexer-result-tokens filename/result))
                   (lexer-result-characters filename/result))]))

(define/contract (char-identifier? x)
  (char? . -> . boolean?)
  (match x
    [(or (? char-alphabetic?) #\_)
     #t]
    [else
     #f]))

(define/contract (read-identifier-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons (? char-identifier?) _)
     (cons (car cs)
           (read-identifier-chars (cdr cs)))]
    [(cons (? char? c) _)
     (list)]))

(module+ test
  (check-equal? (read-identifier-chars (string->list "foo"))
                (list #\f #\o #\o))
  (check-equal? (read-identifier-chars (string->list "foo,"))
                (list #\f #\o #\o)))

(define/contract (read-header-name-chars cs)
  ((listof char?) . -> . (listof char?))
  ;(log-error "read-header-name-chars: ~a" cs)
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons (or (? char-alphabetic?) #\-) _)
     (cons (car cs)
           (read-header-name-chars (cdr cs)))]
    [else
     (list)]))

(define/contract (identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons #\$ cs)
     (define ident-chars (read-identifier-chars cs))
     ;(log-error "read ~a identifier chars: ~a" (length ident-chars) ident-chars)
     (define token-content (token 'IDENTIFIER (list->string ident-chars)))
     (define end-position (add-position start (cons #\$ ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (lexer-result end-position
                   (list id/token)
                   (drop chars (length (cons #\$ ident-chars))))]
    [(cons c _)
     (error (format "Unexpected character (~a) encountered while lexing an identifier at line ~a column ~a."
                    c
                    (position-line start)
                    (position-col start)))]))

(define/contract (environment-identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons (not #\@) _)
     (error (format "Unexpected character (~a) encountered while lexing an identifier at line ~a column ~a."
                    (car chars)
                    (position-line start)
                    (position-col start)))]
    [(cons #\@ cs)
     (define ident-chars (read-identifier-chars cs))
     (define token-content (token 'ENV-IDENTIFIER (list->string ident-chars)))
     (define end-position (add-position start (cons #\@ ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (lexer-result end-position
                   (list id/token)
                   (drop chars (add1 (length ident-chars))))]))

(define/contract (request-header-identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons (not #\^) _)
     (error (format "Unexpected character (~a) encountered while lexing an identifier at line ~a column ~a."
                    (car chars)
                    (position-line start)
                    (position-col start)))]
    [(cons #\^ cs)
     (define ident-chars (read-header-name-chars cs))
     ;(log-error "header name chars: ~a" ident-chars)
     (define token-content (token 'request-header-identifier (list->string ident-chars)))
     (define end-position (add-position start (cons #\^ ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (lexer-result end-position
                   (list id/token)
                   (drop chars (add1 (length ident-chars))))]))

(define/contract (parameter-identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons (not #\%) _)
     (error (format "Unexpected character (~a) encountered while lexing a parameter identifier at line ~a column ~a."
                    (car chars)
                    (position-line start)
                    (position-col start)))]
    [(cons #\% cs)
     (define ident-chars (read-identifier-chars cs))
     (define token-content (token 'parameter (list->string ident-chars)))
     (define end-position (add-position start (cons #\% ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (lexer-result end-position
                   (list id/token)
                   (drop chars (add1 (length ident-chars))))]))

(define/contract (comment chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list #\# (not #\newline) ...)
     (define end-position (add-position start chars))
     (define comment/token (position-token (token 'COMMENT (list->string (cdr chars)) #:skip? #t)
                                           start
                                           end-position))
     (lexer-result end-position
                   (list comment/token)
                   (list))]
    [(list-rest #\# (not #\newline) ... #\newline _)
     (define comment-chars (takef (cdr chars)
                                  (lambda (x)
                                    (not (char=? x #\newline)))))
     (define end-position (add-position start (cons #\# comment-chars)))
     (define comment/token (position-token (token 'COMMENT (list->string comment-chars) #:skip? #t)
                                           start
                                           end-position))
     (lexer-result end-position
                   (list comment/token)
                   (drop chars (length (cons #\# comment-chars))))]))

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
  (define text/token (position-token (token 'URI-TEMPLATE-LITERAL (list->string text-chars))
                                     start
                                     end-position))
  (lexer-result end-position
                (list text/token)
                (drop chars (length text-chars))))

(define/contract (uri-template:template chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "uri template: template: ~a" chars)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (or #\{ #\? #\* #\,) _)
     (define new-position (add-position start (car chars)))
     (define template/token (position-token (token (~a (car chars)))
                                            start
                                            new-position))
     (define more/result (uri-template:template (cdr chars)
                                                new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons template/token (lexer-result-tokens more/result))])]
    [(cons #\} _)
     (define new-position (add-position start #\}))
     (define t (position-token (token "}")
                               start
                               new-position))
     (lexer-result new-position
                   (list t)
                   (cdr chars))]
    [(cons (? char-alphabetic?) _)
     (define identifier-chars (takef chars char-identifier?))
     (define new-position (add-position start identifier-chars))
     (define id/token (position-token (token 'IDENTIFIER (list->string identifier-chars))
                                      start
                                      new-position))
     (define more/result (uri-template:template (drop chars (length identifier-chars))
                                                new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons id/token (lexer-result-tokens more/result))])]))

(define/contract (uri-template chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "uri-template: ~a" chars)
  (match chars
    [(list (? char-whitespace?) ...)
     (lexer-result start
                   (list)
                   (list))]
    [(cons #\{ _)
     (define template/result (uri-template:template chars start))
     (match (lexer-result-characters template/result)
       [(list)
        template/result]
       [(cons (? char-whitespace?) _)
        template/result]
       [(cons #\{ _)
        (define remainder/result (uri-template:template (lexer-result-characters template/result)
                                                        (lexer-result-end-position template/result)))
        (lexer-result (lexer-result-end-position remainder/result)
                      (append (lexer-result-tokens template/result)
                              (lexer-result-tokens remainder/result))
                      (lexer-result-characters remainder/result))]
       [(cons c _)
        (define remainder/result (uri-template (lexer-result-characters template/result)
                                               (lexer-result-end-position template/result)))
        (lexer-result (lexer-result-end-position remainder/result)
                      (append (lexer-result-tokens template/result)
                              (lexer-result-tokens remainder/result))
                      (lexer-result-characters remainder/result))])]
    [(cons (? char-whitespace?) _)
     (uri-template (cdr chars)
                   (add-position start (car chars)))]
    [(cons (not (? char-whitespace?)) _)
     (define text/result (uri-template:text chars start))
     (match (lexer-result-characters text/result)
       [(list)
        text/result]
       [(cons (? char-whitespace?) _)
        text/result]
       [(cons #\{ _)
        (define remainder/result (uri-template (lexer-result-characters text/result)
                                               (lexer-result-end-position text/result)))
        (lexer-result (lexer-result-end-position remainder/result)
                      (append (lexer-result-tokens text/result)
                              (lexer-result-tokens remainder/result))
                      (lexer-result-characters remainder/result))]
       [(cons c _)
        (error (format "Unexpected character (~a) found while lexing a URI template at line ~a column ~a"
                       c
                       (position-line (lexer-result-end-position text/result))
                       (position-col (lexer-result-end-position text/result))))])]))

(define/contract (read-json-pointer-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons _ _)
     (cons (car cs)
           (read-json-pointer-chars (cdr cs)))]))

(define/contract (json-pointer chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define jp-chars (read-json-pointer-chars chars))
  (define new-position (add-position start jp-chars))
  (define jp/token (position-token (token 'JSON-POINTER (list->string jp-chars))
                                   start
                                   new-position))
  (lexer-result new-position
                (list jp/token)
                (drop chars (length jp-chars))))

(define/contract (json-string chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "json-string: ~a" chars)
  (match chars
    [(list-rest #\" (not #\") ... #\" _)
     (define idx-of-double-quote (index-of (cdr chars) #\" char=?))
     (define string-chars (take (cdr chars) idx-of-double-quote))
     ;(log-error "string-chars: ~a" string-chars)
     (define new-position (add-position start (cons #\" string-chars)))
     (define string/token (position-token (token 'JSON-STRING (list->string string-chars))
                                          start
                                          new-position))
     (define remaining-chars (drop chars (+ idx-of-double-quote 2)))
     ;(log-error "remaining chars: ~a" remaining-chars)
     (lexer-result new-position
                   (list string/token)
                   remaining-chars)]))

(module+ test
  #;(json-string (string->list "\"hi\""))
  )

(define/contract (read-integer-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (cons (car cs)
           (read-integer-chars (cdr cs)))]
    [else
     (list)]))

(define/contract (number chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define digits (read-integer-chars chars))
  (when (empty? chars)
    (error "Failed to read any integer characters!"))
  (define remaining-chars (drop chars (length digits)))
  (define end-position (add-position start digits))
  (match remaining-chars
    [(list)
     (lexer-result end-position
                   (list (position-token (token 'number (string->number (list->string digits)))
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
       (define number/token (position-token (token 'number (string->number n))
                                            start
                                            end-pos-after-dot))
       (lexer-result end-pos-after-dot
                     (list number/token)
                     (drop chars (length (append digits (list #\.) after-decimal-digits)))))]
    [else
     (define number/token (position-token (token 'number (string->number (list->string digits)))
                                          start
                                          end-position))
     (lexer-result end-position
                   (list number/token)
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
  ;(log-error "eat-whitespace: ~a" chars)
  (define (consume cs)
    ;(log-error "consume: ~a" cs)
    (match cs
      [(list)
       (list)]
      [(cons (? char-whitespace? c) _)
       (cons c (consume (cdr cs)))]
      [else
       (list)]))
  (define consumed (consume chars))
  ;(log-error "consumed ~a whitespace chars" (length consumed))
  ;(log-error "consumed chars: ~a" consumed)
  (define remaining-chars (drop chars (length consumed)))
  ;(log-error "remaining characters: ~a" remaining-chars)
  (define new-position (add-position start consumed))
  ;(log-error "new position: ~a" new-position)
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

(define/contract (lex-jsonish-stuff chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "lex-jsonish: ~a" chars)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(list #\$ (or (? char-upper-case?) (? char-lower-case?)) ..1 (? char-whitespace?) ...)
     (define new-position (add-position start chars))
     (define id/token (position-token (token 'IDENTIFIER (list->string (cdr chars)))
                                      start
                                      new-position))
     (lexer-result new-position
                   (list id/token)
                   (list))]
    [(list-rest #\$ (or (? char-upper-case?) (? char-lower-case?)) ..1 (? char-whitespace?) ... (not (? char-whitespace?)) _)
     (define up-to-whitespace (takef chars (negate char-whitespace?)))
     ;(log-error "up-to-whitespace: ~a" up-to-whitespace)
     (define new-position (add-position start up-to-whitespace))
     (define id/token (position-token (token 'IDENTIFIER (list->string (cdr up-to-whitespace)))
                                      start
                                      new-position))
     (define more/result (lex-jsonish-stuff (drop chars (length up-to-whitespace))
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons id/token (lexer-result-tokens more/result))])]
    [(cons (? char-whitespace? c) _)
     (lex-jsonish-stuff (cdr chars)
                        (add-position start c))]
    [(cons (or #\{ #\} #\[ #\] #\, #\:) _)
     (define new-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol (~a (car chars))))
                               start
                               new-position))
     (define more/result (lex-jsonish-stuff (cdr chars)
                                            new-position))
     (lexer-result (lexer-result-end-position more/result)
                   (cons t (lexer-result-tokens more/result))
                   (lexer-result-characters more/result))]
    [(cons #\" _)
     (define string/result (json-string chars start))
     (define more/result (lex-jsonish-stuff (lexer-result-characters string/result)
                                            (lexer-result-end-position string/result)))
     (lexer-result (lexer-result-end-position more/result)
                   (append (lexer-result-tokens string/result)
                           (lexer-result-tokens more/result))
                   (lexer-result-characters more/result))]
    [(list #\n #\u #\l #\l (? char-whitespace?) ...)
     (define new-position (add-position start (take chars 4)))
     (lexer-result new-position
                   (list (position-token (token 'json-null)
                                         start
                                         new-position))
                   (list))]
    [(list #\t #\r #\u #\e (? char-whitespace?) ...)
     (define new-position (add-position start (take chars 4)))
     (lexer-result new-position
                   (list (position-token (token 'json-true)
                                         start
                                         new-position))
                   (list))]
    [(list #\f #\a #\l #\s #\e (? char-whitespace?) ...)
     (define new-position (add-position start (take chars 5)))
     (lexer-result new-position
                   (list (position-token (token 'json-true)
                                         start
                                         new-position))
                   (list))]
    [(list-rest #\n #\u #\l #\l (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 4)))
     (define null-token (position-token (token 'json-null)
                                        start
                                        new-position))
     (define more/result (lex-jsonish-stuff (drop chars 4)
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons null-token (lexer-result-tokens more/result))])]
    [(list-rest #\t #\r #\u #\e (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 4)))
     (define null-token (position-token (token 'json-true)
                                        start
                                        new-position))
     (define more/result (lex-jsonish-stuff (drop chars 4)
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons null-token (lexer-result-tokens more/result))])]
    [(list-rest #\f #\a #\l #\s #\e (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 5)))
     (define null-token (position-token (token 'json-false)
                                        start
                                        new-position))
     (define more/result (lex-jsonish-stuff (drop chars 5)
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons null-token (lexer-result-tokens more/result))])]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (define number/result (number chars start))
     (define more/result (lex-jsonish-stuff (lexer-result-characters number/result)
                                            (lexer-result-end-position number/result)))
     (lexer-result (lexer-result-end-position more/result)
                   (append (lexer-result-tokens number/result)
                           (lexer-result-tokens more/result))
                   (lexer-result-characters more/result))]
    [else
     (lexer-result start
                   (list)
                   chars)]))

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
  ;(log-error "read-http-method-chars: ~a" cs)
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
  (define method/token (position-token (token 'HTTP-METHOD (list->string method-chars))
                                       start
                                       position-after-method))
  (define remaining-characters (drop chars (length method-chars)))
  (lexer-result position-after-method
                (list method/token)
                remaining-characters))

(define/contract (after-http-method-payload chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "after-http-method-payload: ~a" chars)
  (match chars
    [(cons (? char-whitespace? c) _)
     (after-http-method-payload (cdr chars)
                                (add-position start c))]
    [(list #\t #\o (? char-whitespace?) ...)
     (define new-position (add-position start chars))
     (define t (position-token (token 'to)
                               start
                               new-position))
     (lexer-result new-position
                   (list t)
                   (list))]
    [(list-rest #\t #\o (? char-whitespace?) _)
     (define to/result (consume-keyword "to" chars start))
     (define uri-template/result (uri-template (lexer-result-characters to/result)
                                               (lexer-result-end-position to/result)))
     (struct-copy lexer-result
                  uri-template/result
                  [tokens (append (lexer-result-tokens to/result)
                                  (lexer-result-tokens uri-template/result))])]))

(define/contract (after-http-method chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "after-http-method: ~a" chars)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (? char-whitespace?) _)
     (after-http-method (cdr chars)
                        (add-position start (car chars)))]
    ;; disambiguate between a JSON object an a URI template:
    ;; case: empty JSON object
    [(list-rest #\{ (? char-whitespace?) ... #\} (? char-whitespace?) #\t #\o _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    ;; case: non-empty JSON object
    [(list-rest #\{ (? char-whitespace?) ... #\" _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    ;; starts with {, but does not look like a JSON object. Decided: it's a URI Template
    [(cons #\{ _)
     (uri-template chars start)]
    [(cons (or #\$ #\[ #\") _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest #\t #\r #\u #\e (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest #\f #\a #\l #\s #\e (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))    ]
    [(list-rest #\n #\u #\l #\l (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [(list-rest (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 #\. (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 (? char-whitespace?) ..1 #\t #\o (? char-whitespace?) ..1 _)
     (define jsony/result (lex-jsonish-stuff chars start))
     (define whatever/result (after-http-method-payload (lexer-result-characters jsony/result)
                                                        (lexer-result-end-position jsony/result)))
     (lexer-result (lexer-result-end-position whatever/result)
                   (append (lexer-result-tokens jsony/result)
                           (lexer-result-tokens whatever/result))
                   (lexer-result-characters whatever/result))]
    [else
     (uri-template chars start)]))

(define/contract (http-method chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (http-method (cdr chars)
                  (add-position start (car chars)))]
    [(cons (? char-upper-case?) _)
     (define method-chars (takef chars char-upper-case?))
     (define method-end-position (add-position start method-chars))
     (define after-method-chars (drop chars (length method-chars)))
     (define method/token (position-token (token 'HTTP-METHOD (list->string method-chars))
                                          start
                                          method-end-position))
     (define after-method/result (after-http-method after-method-chars method-end-position))
     (lexer-result (lexer-result-end-position after-method/result)
                   (cons method/token (lexer-result-tokens after-method/result))
                   (lexer-result-characters after-method/result))]))

#;
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
  (let ([result (http-method (string->list "GET bar") (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "GET") (position 1 1 0) (position 4 1 3))
                   (position-token
                    '(uri-template-text . "bar")
                    (position 5 1 4)
                    (position 8 1 7)))))
  (let* ([program "POST { \"hi\": \"there\" } to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (log-error "~a" program)
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token '|{| (position 6 1 5) (position 7 1 6))
                   (position-token '(json-string . "hi") (position 8 1 7) (position 11 1 10))
                   (position-token ': (position 11 1 10) (position 12 1 11))
                   (position-token
                    '(json-string . "there")
                    (position 13 1 12)
                    (position 19 1 18))
                   (position-token '|}| (position 20 1 19) (position 21 1 20))
                   (position-token 'to (position 22 1 21) (position 24 1 23))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 25 1 24)
                    (position 33 1 32)))))
  (let* ([program "POST [ 1, 2, 3 ] to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token '|[| (position 6 1 5) (position 7 1 6))
                   (position-token '(number . 1) (position 8 1 7) (position 9 1 8))
                   (position-token '|,| (position 9 1 8) (position 10 1 9))
                   (position-token '(number . 2) (position 11 1 10) (position 12 1 11))
                   (position-token '|,| (position 12 1 11) (position 13 1 12))
                   (position-token '(number . 3) (position 14 1 13) (position 15 1 14))
                   (position-token '|]| (position 16 1 15) (position 17 1 16))
                   (position-token 'to (position 18 1 17) (position 20 1 19))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 21 1 20)
                    (position 29 1 28)))))
  (let* ([program "POST null to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token 'json-null (position 6 1 5) (position 10 1 9))
                   (position-token 'to (position 11 1 10) (position 13 1 12))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 14 1 13)
                    (position 22 1 21)))))
  #;
  (let* ([program "POST false to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token 'json-false (position 6 1 5) (position 11 1 10))
                   (position-token 'to (position 12 1 11) (position 14 1 13))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 15 1 14)
                    (position 23 1 22)))))
  #;
  (let* ([program "POST true to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token 'json-true (position 6 1 5) (position 10 1 9))
                   (position-token 'to (position 11 1 10) (position 13 1 12))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 14 1 13)
                    (position 22 1 21)))))
  #;
  (let* ([program "POST \"hi\" to whatever"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "POST") (position 1 1 0) (position 5 1 4))
                   (position-token '(json-string . "hi") (position 6 1 5) (position 9 1 8))
                   (position-token 'to (position 10 1 9) (position 12 1 11))
                   (position-token
                    '(uri-template-text . "whatever")
                    (position 13 1 12)
                    (position 21 1 20)))))
  #;
  (let* ([program "GET null"]
         [result (http-method (string->list program) (position 1 1 0))])
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "GET") (position 1 1 0) (position 4 1 3))
                   (position-token
                    '(uri-template-text . "null")
                    (position 5 1 4)
                    (position 9 1 8))))))

(define/contract (responds:responds chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define keyword-length (string-length "responds"))
  (define chars-of-keyword (take chars keyword-length))
  (define chars-after-keyword (drop chars keyword-length))
  (define pos-after-keyword (add-position start chars-of-keyword))
  (define responds/token (position-token (token 'responds)
                                         start
                                         pos-after-keyword))
  (match chars
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s _)
     (lexer-result (add-position start (take chars keyword-length))
                   (list responds/token)
                   (drop chars keyword-length))]))

(define/contract (responds:with chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define keyword-length (string-length "with"))
  (define chars-of-keyword (take chars keyword-length))
  (define chars-after-keyword (drop chars keyword-length))
  (define pos-after-keyword (add-position start chars-of-keyword))
  (define with/token (position-token (token 'with)
                                     start
                                     pos-after-keyword))
  (match chars
    [(list-rest #\w #\i #\t #\h _)
     (lexer-result pos-after-keyword
                   (list with/token)
                   (drop chars keyword-length))]))

(define/contract (responds:status-code chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                _)
     (define chars-of-keyword (take chars 3))
     (define chars-after-keyword (drop chars 3))
     (define pos-after-keyword (add-position start chars-of-keyword))
     (define token-content (list->string chars-of-keyword))
     (define code/token (position-token (token 'HTTP-STATUS-CODE token-content)
                                        start
                                        pos-after-keyword))
     (lexer-result pos-after-keyword
                   (list code/token)
                   chars-after-keyword)]))

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
                                    (lexer-result-tokens status-code/result))
                            (lexer-result-characters status-code/result))])])])]))

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
  (position-token (token 'http-status-code (list->string (list first-char second-char third-char)))
                  (position c1 l1 c1)
                  (position c2 l2 c2)))

(define/contract (consume-keyword keyword chars start)
  (string? (listof char?) position? . -> . lexer-result?)
  (define string-chars (string->list keyword))
  (define initial-segment (take chars (length string-chars)))
  (unless (= (length initial-segment) (length string-chars))
    (error (format "Too few characters available! Cannot lex \"~a\" because all we have are ~a"
                   keyword
                   chars)))
  (define end-position (add-position start initial-segment))
  (define keyword/token (position-token (token (string->symbol keyword))
                                        start
                                        end-position))
  (lexer-result end-position
                (list keyword/token)
                (drop chars (length initial-segment))))

(define/contract (to chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\t #\o _)
     (define end-position (add-position start (take chars 2)))
     (lexer-result end-position
                   (list (position-token (token 'to)
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
                   (list (position-token (token (string->symbol ":="))
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

(define (base-parameter chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\% #\b #\a #\s #\e (? char-whitespace?) ..1
                #\: #\= (? char-whitespace?) ..1
                (not (? char-whitespace?)) ..1
                _)
     (define parameter-chars (take (cdr chars) 4))
     (define new-position (add-position start (take chars 5)))
     (define parameter-token (position-token (token 'parameter (list->string parameter-chars))
                                             start
                                             new-position))
     (define after-parameter-chars (drop chars 5))
     (define after-parameter-whitespace (takef after-parameter-chars char-whitespace?))
     (define before-assignment-position
       (add-position new-position after-parameter-whitespace))
     (define after-assignment-position
       (add-position before-assignment-position (list #\: #\=)))
     (define assignment-token (position-token (token (string->symbol ":="))
                                              before-assignment-position
                                              after-assignment-position))
     (define after-assignment-chars (drop after-parameter-chars (+ (length after-parameter-whitespace)
                                                                   (length (list #\: #\=)))))
     (define after-assignment-whitespace (takef after-assignment-chars char-whitespace?))
     (define after-assignment-whitespace-position
       (add-position after-assignment-position after-assignment-whitespace))
     (define template/result (uri-template after-assignment-chars
                                           after-assignment-position))
     (lexer-result (lexer-result-end-position template/result)
                   (append (list parameter-token
                                 assignment-token)
                           (lexer-result-tokens template/result))
                   (lexer-result-characters template/result))]))

(define/contract (in chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list #\i #\n (? char-whitespace?) ...)
     (consume-keyword "in" chars start)]
    [(list-rest #\i #\n (? char-whitespace?) ..1 (not (? char-whitespace?)) _)
     (define in/result (consume-keyword "in" chars start))
     (define filename/result (import-filename (lexer-result-characters in/result)
                                              (lexer-result-end-position in/result)))
     (lexer-result (lexer-result-end-position filename/result)
                   (append (lexer-result-tokens in/result)
                           (lexer-result-tokens filename/result))
                   (lexer-result-characters filename/result))]))

(define/contract (at chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list #\a #\t (? char-whitespace?) ...)
     (consume-keyword "at" chars start)]
    [(list #\a #\t (? char-whitespace?) ..1 (not (? char-whitespace?)))
     (define at/result (consume-keyword "at" chars start))
     (define uri/result (uri-template (lexer-result-characters at/result)
                                      (lexer-result-end-position at/result)))
     (lexer-result (lexer-result-end-position uri/result)
                   (append (lexer-result-tokens at/result)
                           (lexer-result-tokens uri/result))
                   (lexer-result-characters uri/result))]))

(define/contract (initial chars start)
  ((listof char?) position? . -> . (listof position-token?))
  (match chars
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (initial (cdr chars)
              (add-position start (car chars)))]
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
    [(cons #\@ _)
     (define result (environment-identifier chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\^ _)
     (define result (request-header-identifier chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\% #\b #\a #\s #\e _)
     (define result (base-parameter chars start))
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
    [(list-rest #\! #\= _)
     (define result (consume-keyword "!=" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (? char-upper-case?) _)
     (define result (http-method chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\" #\{ #\} #\[ #\]) _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\= #\< #\> #\+ #\-) _)
     (define result (consume-keyword (~a (car chars)) chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\i #\s _)
     (define result (consume-keyword "is" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\n #\o #\t _)
     (define result (consume-keyword "not" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\n #\o #\n _)
     (define result (consume-keyword "non" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\m #\p #\t #\y _)
     (define result (consume-keyword "empty" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\d #\o #\e #\s _)
     (define result (consume-keyword "does" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s _)
     (define result (responds chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\r #\e #\l #\a #\t #\i #\v #\e _)
     (define result (consume-keyword "relative" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\t #\o _)
     (define result (consume-keyword "to" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list #\e #\x #\i #\s #\t (? char-whitespace?) ...)
     (define result (consume-keyword "exist" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\x #\i #\s #\t (not #\s) _)
     (define result (consume-keyword "exist" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\x #\i #\s #\t #\s _)
     (define result (consume-keyword "exists" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\a #\n #\d _)
     (define result (consume-keyword "and" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\s #\a #\t #\i #\s #\f #\i #\e #\s _)
     (define result (consume-keyword "satisfies" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\s #\c #\h #\e #\m #\a _)
     (define result (consume-keyword "schema" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\i #\n _)
     (define result (in chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\w #\i #\t #\h _)
     (define result (consume-keyword "with" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\f #\a #\l #\l #\b #\a #\c #\k _)
     (define result (consume-keyword "fallback" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\c #\h #\o _)
     (define result (consume-keyword "echo" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (? char? c) _)
     (error (format "Unexpected character (~a) encountered at the toplevel at line ~a column ~a. Bailing out."
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
         (define (toke-it ip)
           (tokenize (port->chars ip) start))
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
    (check-equal?
     (tokenize program)
     (list
      (position-token 'import (position 1 1 0) (position 7 1 6))
      (position-token '(filename . "base.rip") (position 8 1 7) (position 16 1 15))
      (position-token
       '(comment
         .
         " Test if the v1/search endpoint provides the same price/qty_increments as v1/catalog/products")
       (position 18 3 0)
       (position 112 3 94))
      (position-token
       '(comment . "# v1/search")
       (position 114 5 0)
       (position 126 5 12))
      (position-token
       '(identifier . "searchQueryParams")
       (position 127 6 0)
       (position 145 6 18))
      (position-token ':= (position 146 6 19) (position 148 6 21))
      (position-token '|{| (position 149 6 22) (position 150 6 23))
      (position-token
       '(json-string . "pagination_page")
       (position 155 7 4)
       (position 171 7 20))
      (position-token ': (position 171 7 20) (position 172 7 21))
      (position-token '(json-string . "1") (position 173 7 22) (position 175 7 24))
      (position-token '|,| (position 175 7 24) (position 176 7 25))
      (position-token
       '(json-string . "pagination_limit")
       (position 181 8 4)
       (position 198 8 21))
      (position-token ': (position 198 8 21) (position 199 8 22))
      (position-token '(json-string . "1") (position 200 8 23) (position 202 8 25))
      (position-token '|,| (position 202 8 25) (position 203 8 26))
      (position-token
       '(json-string . "in_stock_vendors")
       (position 208 9 4)
       (position 225 9 21))
      (position-token ': (position 225 9 21) (position 226 9 22))
      (position-token
       '(json-string . "VICAMPO")
       (position 227 9 23)
       (position 235 9 31))
      (position-token '|,| (position 235 9 31) (position 236 9 32))
      (position-token
       '(json-string . "sort")
       (position 241 10 4)
       (position 246 10 9))
      (position-token ': (position 246 10 9) (position 247 10 10))
      (position-token
       '(json-string . "relevance")
       (position 248 10 11)
       (position 258 10 21))
      (position-token '|,| (position 258 10 21) (position 259 10 22))
      (position-token
       '(json-string . "fields")
       (position 264 11 4)
       (position 271 11 11))
      (position-token ': (position 271 11 11) (position 272 11 12))
      (position-token
       '(json-string
         .
         "ratings,expert_rating,type,product_id,campaign_id,name,manufacturer_name,tags,price,stocks,images,legal,customer_rating,customer_rating_count")
       (position 273 11 13)
       (position 415 11 155))
      (position-token '|,| (position 415 11 155) (position 416 11 156))
      (position-token
       '(json-string . "embed")
       (position 421 12 4)
       (position 427 12 10))
      (position-token ': (position 427 12 10) (position 428 12 11))
      (position-token
       '(json-string . "bundle_items,has_been_bought,expert_reviews")
       (position 429 12 12)
       (position 473 12 56))
      (position-token '|}| (position 474 13 0) (position 475 13 1))
      (position-token
       '(http-method . "GET")
       (position 477 15 0)
       (position 480 15 3))
      (position-token
       '(uri-template-text . "search")
       (position 481 15 4)
       (position 487 15 10))
      (position-token '|{| (position 487 15 10) (position 488 15 11))
      (position-token '? (position 488 15 11) (position 489 15 12))
      (position-token
       '(identifier . "searchQueryParams")
       (position 489 15 12)
       (position 506 15 29))
      (position-token '* (position 506 15 29) (position 507 15 30))
      (position-token '|}| (position 507 15 30) (position 508 15 31))))))

(module+ test
  (let ([program #<<RIPOSTE
import base.rip
import token-auth.rip

POST {} to customers/addresses responds with 422

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

POST $validData to customers/addresses responds with 201

/customer_address_id exists
RIPOSTE
                 ])
    (check-equal?
     (tokenize program)
     (list))))

#;
(module+ test
  (let ([program #<<RIPOSTE
import base.rip
import token-auth.rip

$uuid := @UUID with fallback "4526554c-fc86-42ad-aeb4-57c8f4cb5674"

GET checkout/cart/{uuid} responds with 2XX

# Add something to the cart:

$productData := {
  "product_id": 13698,
  "campaign_id": 1,
  "qty": 2
}

$gourmetData := {
  "product_id": 60868, # this should be a gourmet item!
  "campaign_id": 1,
  "qty": 2
}

POST $productData to checkout/cart/{uuid}/items responds with 2XX

######################################################################
# Sanity check: the cart grand total is now not zero:
######################################################################

$initialGrandTotal := /grand_total

$initialGrandTotal > 0

######################################################################
# Adding an item will increase the grand total
######################################################################

POST $productData to checkout/cart/{uuid}/items responds with 2XX

$initialGrandTotalTwo := /grand_total

$initialGrandTotalTwo > 0

$initialGrandTotalTwo > $initialGrandTotal

######################################################################
# Adding a gourmet item will not increase the grand total
######################################################################

POST $gourmetData to checkout/cart/{uuid}/items responds with 2XX

$initialGrandTotalThree := /grand_total

$initialGrandTotalThree > 0

$initialGrandTotalTwo = $initialGrandTotalThree
RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

#;
(module+ test
  (let ([program #<<RIPOSTE
import base.rip
import token-auth.rip

$uuid := @UUID with fallback "4526554c-fc86-42ad-aeb4-57c8f4cb5674"

GET checkout/cart/{uuid} responds with 2XX

/undiscounted_grand_total exists
/undiscounted_tax_total exists
/undiscounted_shipping_total exists

# This is deprecated, but still there:
/shipping_cost exists

/discounts exists

# Add something to the cart:

$productData := {
  "product_id": 41966,
  "campaign_id": 1,
  "qty": 10
}

POST $productData to checkout/cart/{uuid}/items responds with 2XX

$initialGrandTotal := /grand_total

######################################################################
# Sanity check: the cart grand total is now not zero:
######################################################################

GET checkout/cart/{uuid} responds with 2XX

/grand_total > 0

######################################################################
# Now try to use a coupon with the new approach:
######################################################################

$couponQueryData := { "apply_coupon_code": "TEST10" }

GET checkout/cart/{uuid}/{?couponQueryData*} responds with 200

# These fields were added in VIP-3586:

/discounts exists
/discounts/0/type exists
/discounts/0/type = "coupon"

/discounts/0/label exists
/discounts/0/label = "coupons"

/discounts/0/amount exists
/discounts/0/amount = 10 # the value of the coupon
/grand_total + 10 = $initialGrandTotal # the grand total is smaller

######################################################################
# Use our credits
######################################################################

$creditData := { "apply_credits": "1" }

GET checkout/cart/{uuid}/{?creditData*} responds with 200

# The new fields should be there, but with different values than
# we received previously:

/discounts exists
/discounts/0/type exists
/discounts/0/type = "credit"

/discounts/0/label exists
/discounts/0/label = "credits"

/discounts/0/amount exists
/discounts/0/amount > 0 # the value of our credits
/grand_total < $initialGrandTotal # the grand total is smaller if we use credits

######################################################################
#
# If we try to use both at the same time, we should get an error:
#
######################################################################

$badDiscountData := {
  "apply_coupon_code": "TEST10",
  "apply_credits": "1"
}

GET checkout/cart/{uuid}/{?badDiscountData*} responds with 422

######################################################################
#
# Miles & More should work:
#
######################################################################

$milesData := {
  "apply_miles": 330 # worth exactly 1 euro at current exchange rate
}

GET checkout/cart/{uuid}/{?milesData*} responds with 2XX

/discounts exists
/discounts/0/type exists
/discounts/0/type = "miles"

/discounts/0/label exists
/discounts/0/label = "milesandmore"

/discounts/0/amount exists
/discounts/0/amount > 0 # the value of our credits
/grand_total + 1 = $initialGrandTotal # we saved 1 EUR

######################################################################
#
# Combining miles and a coupon works:
#
######################################################################

$milesAndCouponData := {
  "apply_miles": 330, # worth exactly 1 euro at current exchange rate
  "apply_coupon_code": "TEST10"
}

GET checkout/cart/{uuid}/{?milesAndCouponData*} responds with 2XX

/grand_total + 11 = $initialGrandTotal # we saved 11 = 10 + 1 EUR

RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

#;
(module+ test
  (let ([program #<<RIPOSTE
# value doesn't matter; header just needs to be present
^Authorization := ""

# You'll find this in local.php:
^X-Auth-Access-Token := @AUTH_ACCESS_TOKEN with fallback ""

# Your API consumer doppelgänger from the api_consumer table
# (column external_consumer_id):
^X-Consumer-Id := @CONSUMER_ID with fallback ""

RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

#;
(module+ test
  (let ([program #<<RIPOSTE
%base := https://api.vicampo.test:8443/v1/

# all requests should be understood as JSON
#
# (Requests that have an empty body will also have this header.)

^Content-Type := "application/json"

RIPOSTE
                 ])
    (check-equal? (tokenize program)
                  (list))))

(module+ main

  (require racket/cmdline
           racket/pretty)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (define file-to-process/path
    (string->path file-to-process))

  (unless (file-exists? file-to-process/path)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (pretty-print
   (tokenize file-to-process/path)))
