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
                  index-of )
         (only-in racket/function
                  negate)
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
     (define token (position-token (cons 'filename (list->string chars))
                                   start
                                   new-position))
     (lexer-result new-position
                   (list token)
                   (list))]
    [(list-rest (not (? char-whitespace?)) ..1 (? char-whitespace?) _)
     (define filename-chars (takef chars (negate char-whitespace?)))
     (define filename (list->string filename-chars))
     (define end-position (add-position start filename-chars))
     (define remaining-chars (drop chars (length filename-chars)))
     (define token (position-token (cons 'filename filename)
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
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
     (define filename/result (import-filename remaining-chars
                                              end-position))
     (lexer-result (lexer-result-end-position filename/result)
                   (cons import/token
                         (lexer-result-tokens filename/result))
                   (lexer-result-characters filename/result))]))

(define/contract (read-identifier-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(list)
     (list)]
    [(cons (? char-whitespace?) _)
     (list)]
    [(cons (? char-alphabetic? c) _)
     (cons c (read-identifier-chars (cdr cs)))]
    [(cons (? char? c) _)
     (list)]))

(define/contract (identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Unexpected end-of-file found!")]
    [(cons #\$ cs)
     (define ident-chars (read-identifier-chars cs))
     ;(log-error "read ~a identifier chars: ~a" (length ident-chars) ident-chars)
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
     (define token-content (cons 'identifier (list->string ident-chars)))
     (define end-position (add-position start (cons #\@ ident-chars)))
     (define token (position-token token-content
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
                   (drop chars (add1 (length ident-chars))))]))

(define/contract (comment chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list #\# (not #\newline) ...)
     (define end-position (add-position start chars))
     (define token (position-token (cons 'comment (list->string (cdr chars)))
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
                   (list))]
    [(list-rest #\# (not #\newline) ... #\newline _)
     (define comment-chars (takef (cdr chars)
                                  (lambda (x)
                                    (not (char=? x #\newline)))))
     (define end-position (add-position start (cons #\# comment-chars)))
     (define token (position-token (cons 'comment (list->string comment-chars))
                                   start
                                   end-position))
     (lexer-result end-position
                   (list token)
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
  (define token (position-token (cons 'uri-template-text (list->string text-chars))
                                start
                                end-position))
  (lexer-result end-position
                (list token)
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
     (define token (position-token (string->symbol (~a (car chars)))
                                   start
                                   new-position))
     (define more/result (uri-template:template (cdr chars)
                                                new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons token (lexer-result-tokens more/result))])]
    [(cons #\} _)
     (define new-position (add-position start #\}))
     (define token (position-token (string->symbol "}")
                                   start
                                   new-position))
     (lexer-result new-position
                   (list token)
                   (cdr chars))]
    [(cons (? char-alphabetic?) _)
     (define identifier-chars (takef chars char-alphabetic?))
     (define new-position (add-position start identifier-chars))
     (define token (position-token (cons 'identifier (list->string identifier-chars))
                                   start
                                   new-position))
     (define more/result (uri-template:template (drop chars (length identifier-chars))
                                                new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons token (lexer-result-tokens more/result))])]))

(define/contract (uri-template chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "uri-template: ~a" chars)
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
    [(cons (? char-whitespace?) _)
     (lexer-result start
                   (list)
                   chars)]
    [(cons (not (? char-whitespace?)) _)
     (define text/result (uri-template:text chars start))
     (define remainder/result (uri-template (lexer-result-characters text/result)
                                            (lexer-result-end-position text/result)))
     (lexer-result (lexer-result-end-position remainder/result)
                   (append (lexer-result-tokens text/result)
                           (lexer-result-tokens remainder/result))
                   (lexer-result-characters remainder/result))]))

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
  (define token (position-token (cons 'json-pointer (list->string jp-chars))
                                start
                                new-position))
  (lexer-result new-position
                (list token)
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
     (define token (position-token (cons 'json-string (list->string string-chars))
                                   start
                                   new-position))
     (define remaining-chars (drop chars (+ idx-of-double-quote 2)))
     ;(log-error "remaining chars: ~a" remaining-chars)
     (lexer-result new-position
                   (list token)
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
                   (list (position-token (cons 'number (string->number (list->string digits)))
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
     (define token (position-token (cons 'number (string->number (list->string digits)))
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
    [(list #\$ (or (? char-upper-case?) (? char-lower-case?)) ..1)
     (define new-position (add-position start chars))
     (define token (position-token (cons 'identifier (list->string (cdr chars)))
                                   start
                                   new-position))
     (lexer-result new-position
                   (list token)
                   (list))]
    [(list-rest #\$ (or (? char-upper-case?) (? char-lower-case?)) ..1 (? char-whitespace?) _)
     (define up-to-whitespace (takef chars (negate char-whitespace?)))
     ;(log-error "up-to-whitespace: ~a" up-to-whitespace)
     (define new-position (add-position start up-to-whitespace))
     (define token (position-token (cons 'identifier (list->string (cdr up-to-whitespace)))
                                   start
                                   new-position))
     (define more/result (lex-jsonish-stuff (drop chars (length up-to-whitespace))
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons token (lexer-result-tokens more/result))])]
    [(cons (? char-whitespace? c) _)
     (lex-jsonish-stuff (cdr chars)
                        (add-position start c))]
    [(cons (or #\{ #\} #\[ #\] #\, #\:) _)
     (define new-position (add-position start (car chars)))
     (define token (position-token (string->symbol (~a (car chars)))
                                   start
                                   new-position))
     (define more/result (lex-jsonish-stuff (cdr chars)
                                            new-position))
     (lexer-result (lexer-result-end-position more/result)
                   (cons token (lexer-result-tokens more/result))
                   (lexer-result-characters more/result))]
    [(cons #\" _)
     (define string/result (json-string chars start))
     (define more/result (lex-jsonish-stuff (lexer-result-characters string/result)
                                            (lexer-result-end-position string/result)))
     (lexer-result (lexer-result-end-position more/result)
                   (append (lexer-result-tokens string/result)
                           (lexer-result-tokens more/result))
                   (lexer-result-characters more/result))]
    [(list #\n #\u #\l #\l)
     (define new-position (add-position start (take chars 4)))
     (lexer-result new-position
                   (list (position-token 'json-null
                                         start
                                         new-position))
                   (list))]
    [(list #\t #\r #\u #\e)
     (define new-position (add-position start (take chars 4)))
     (lexer-result new-position
                   (list (position-token 'json-true
                                         start
                                         new-position))
                   (list))]
    [(list #\f #\a #\l #\s #\e)
     (define new-position (add-position start (take chars 5)))
     (lexer-result new-position
                   (list (position-token 'json-true
                                         start
                                         new-position))
                   (list))]
    [(list-rest #\n #\u #\l #\l (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 4)))
     (define null-token (position-token 'json-null
                                        start
                                        new-position))
     (define more/result (lex-jsonish-stuff (drop chars 4)
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons null-token (lexer-result-tokens more/result))])]
    [(list-rest #\t #\r #\u #\e (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 4)))
     (define null-token (position-token 'json-true
                                        start
                                        new-position))
     (define more/result (lex-jsonish-stuff (drop chars 4)
                                            new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons null-token (lexer-result-tokens more/result))])]
    [(list-rest #\f #\a #\l #\s #\e (or (? char-whitespace?) #\,) _)
     (define new-position (add-position start (take chars 5)))
     (define null-token (position-token 'json-false
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

(define/contract (after-http-method-payload chars start)
  ((listof char?) position? . -> . lexer-result?)
  (log-error "after-http-method-payload: ~a" chars)
  (match chars
    [(cons (? char-whitespace? c) _)
     (after-http-method-payload (cdr chars)
                                (add-position start c))]
    [(list #\t #\o)
     (define new-position (add-position start chars))
     (define token (position-token 'to
                                   start
                                   new-position))
     (lexer-result new-position
                   (list token)
                   (list))]
    [(list-rest #\t #\o (? char-whitespace?) _)
     (define new-position (add-position start (take chars 2)))
     (define token (position-token 'to
                                   start
                                   new-position))
     (define after-keyword (drop chars 2))
     (define after-keyword-and-whitespace (dropf after-keyword char-whitespace?))
     (define uri-template/result (uri-template after-keyword-and-whitespace
                                               (add-position new-position
                                                             (takef after-keyword char-whitespace?))))
     (struct-copy lexer-result
                  uri-template/result
                  [tokens (cons token (lexer-result-tokens uri-template/result))])]))

(define/contract (after-http-method chars start)
  ((listof char?) position? . -> . lexer-result?)
  ;(log-error "after-http-method: ~a" chars)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (? char-whitespace? c) _)
     (after-http-method (cdr chars)
                        (add-position start c))]
    [(cons (or #\$ #\{ #\[ #\") _)
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
                   (cons method/token (lexer-result-tokens after-method/result))
                   (lexer-result-characters after-method/result))]))

(module+ test
  #;
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
    (check-equal? (lexer-result-tokens result)
                  (list
                   (position-token '(http-method . "GET") (position 1 1 0) (position 4 1 3))
                   (position-token
                    '(uri-template-text . "bar")
                    (position 5 1 4)
                    (position 8 1 7)))))
  #;
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
  #;
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
  #;
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
  (match chars
    [(list-rest (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\x #\X)
                _)
     (define chars-of-keyword (take chars 3))
     (define chars-after-keyword (drop chars 3))
     (define pos-after-keyword (add-position start chars-of-keyword))
     (define token-content (list->string chars-of-keyword))
     (define token (position-token (cons 'http-status-code token-content)
                                   start
                                   pos-after-keyword))
     (lexer-result pos-after-keyword
                   (list token)
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

(define/contract (consume-keyword keyword chars start)
  (string? (listof char?) position? . -> . lexer-result?)
  (define string-chars (string->list keyword))
  (define initial-segment (take chars (length string-chars)))
  (unless (= (length initial-segment) (length string-chars))
    (error (format "Too few characters available! Cannot lex \"~a\" because all we have are ~a"
                   keyword
                   chars)))
  (define end-position (add-position start initial-segment))
  (define token (position-token (string->symbol keyword)
                                start
                                end-position))
  (lexer-result end-position
                (list token)
                (drop chars (length initial-segment))))

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
    [(cons (or #\" #\{ #\} #\[ #\]) _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\= #\< #\> #\+) _)
     (define result (consume-keyword "=" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\r #\e #\s #\p #\o #\n #\d #\s _)
     (define result (responds chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\x #\i #\s #\t #\s _)
     (define result (consume-keyword "exists" chars start))
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
     (list
      (position-token 'import (position 1 1 0) (position 7 1 6))
      (position-token '(filename . "base.rip") (position 8 1 7) (position 16 1 15))
      (position-token 'import (position 17 2 0) (position 23 2 6))
      (position-token
       '(filename . "token-auth.rip")
       (position 24 2 7)
       (position 38 2 21))
      (position-token '(http-method . "POST") (position 40 4 0) (position 44 4 4))
      (position-token '|{| (position 45 4 5) (position 46 4 6))
      (position-token '|}| (position 46 4 6) (position 47 4 7))
      (position-token 'to (position 48 4 8) (position 50 4 10))
      (position-token
       '(uri-template-text . "customers/addresses")
       (position 51 4 11)
       (position 70 4 30))
      (position-token 'responds (position 71 4 31) (position 79 4 39))
      (position-token 'with (position 80 4 40) (position 84 4 44))
      (position-token
       '(http-status-code . "422")
       (position 85 4 45)
       (position 88 4 48))
      (position-token
       '(identifier . "validData")
       (position 90 6 0)
       (position 100 6 10))
      (position-token ':= (position 101 6 11) (position 103 6 13))
      (position-token '|{| (position 104 6 14) (position 105 6 15))
      (position-token
       '(json-string . "first_name")
       (position 110 7 4)
       (position 121 7 15))
      (position-token ': (position 121 7 15) (position 122 7 16))
      (position-token
       '(json-string . "BLA")
       (position 123 7 17)
       (position 127 7 21))
      (position-token '|,| (position 127 7 21) (position 128 7 22))
      (position-token
       '(json-string . "last_name")
       (position 133 8 4)
       (position 143 8 14))
      (position-token ': (position 143 8 14) (position 144 8 15))
      (position-token
       '(json-string . "BLO")
       (position 145 8 16)
       (position 149 8 20))
      (position-token '|,| (position 149 8 20) (position 150 8 21))
      (position-token '(json-string . "city") (position 155 9 4) (position 160 9 9))
      (position-token ': (position 160 9 9) (position 161 9 10))
      (position-token
       '(json-string . "TestCity")
       (position 162 9 11)
       (position 171 9 20))
      (position-token '|,| (position 171 9 20) (position 172 9 21))
      (position-token
       '(json-string . "country")
       (position 177 10 4)
       (position 185 10 12))
      (position-token ': (position 185 10 12) (position 186 10 13))
      (position-token
       '(json-string . "de")
       (position 187 10 14)
       (position 190 10 17))
      (position-token '|,| (position 190 10 17) (position 191 10 18))
      (position-token
       '(json-string . "is_default_billing_address")
       (position 196 11 4)
       (position 223 11 31))
      (position-token ': (position 223 11 31) (position 224 11 32))
      (position-token '(number . 1) (position 225 11 33) (position 226 11 34))
      (position-token '|,| (position 226 11 34) (position 227 11 35))
      (position-token
       '(json-string . "is_default_shipping_address")
       (position 232 12 4)
       (position 260 12 32))
      (position-token ': (position 260 12 32) (position 261 12 33))
      (position-token '(number . 1) (position 262 12 34) (position 263 12 35))
      (position-token '|,| (position 263 12 35) (position 264 12 36))
      (position-token
       '(json-string . "street_name")
       (position 269 13 4)
       (position 281 13 16))
      (position-token ': (position 281 13 16) (position 282 13 17))
      (position-token
       '(json-string . "teststreet")
       (position 283 13 18)
       (position 294 13 29))
      (position-token '|,| (position 294 13 29) (position 295 13 30))
      (position-token
       '(json-string . "street_number")
       (position 300 14 4)
       (position 314 14 18))
      (position-token ': (position 314 14 18) (position 315 14 19))
      (position-token '(number . 33) (position 316 14 20) (position 318 14 22))
      (position-token '|,| (position 318 14 22) (position 319 14 23))
      (position-token
       '(json-string . "zip")
       (position 324 15 4)
       (position 328 15 8))
      (position-token ': (position 328 15 8) (position 329 15 9))
      (position-token '(number . 32145) (position 330 15 10) (position 335 15 15))
      (position-token '|}| (position 336 16 0) (position 337 16 1))
      (position-token
       '(http-method . "POST")
       (position 339 18 0)
       (position 343 18 4))
      (position-token
       '(identifier . "validData")
       (position 344 18 5)
       (position 354 18 15))
      (position-token 'to (position 355 18 16) (position 357 18 18))
      (position-token
       '(uri-template-text . "customers/addresses")
       (position 358 18 19)
       (position 377 18 38))
      (position-token 'responds (position 378 18 39) (position 386 18 47))
      (position-token 'with (position 387 18 48) (position 391 18 52))
      (position-token
       '(http-status-code . "201")
       (position 392 18 53)
       (position 395 18 56))
      (position-token
       '(json-pointer . "/customer_address_id")
       (position 397 20 0)
       (position 417 20 20))
      (position-token 'exists (position 418 20 21) (position 424 20 27))))))

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
