#lang racket/base

(provide tokenize)

(require racket/contract
         racket/match
         racket/format
         (only-in racket/string
                  non-empty-string?)
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
  (require rackunit
           syntax/parse/define
           syntax/parse)
  (require (for-syntax racket/base
                       syntax/location))
  (define-syntax (check-tokenize stx)
    (syntax-parse stx
      [(_ program result)
       #`(with-check-info*
           (list (make-check-location
                  (list #,(syntax-source-file-name #'program)
                        #,(syntax-line #'program)
                        #,(syntax-column #'program)
                        #f
                        #f)))
           (lambda ()
             (check-equal? (tokenize program) result)))])))

(define (header-char? x)
  (define i (char->integer x))
  (or (= i 45) ; dash
      (and (> i 47) (< i 58)) ; digits
      (and (> i 64) (< i 91)) ; uppercase digits
      (and (> i 96) (< i 123))))

#|

Identifiers: $ followed by a sequence of letters, numbers, '_', and "-"

|#

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

(define/contract (import chars start)
  ((listof char?) position? . -> . lexer-result?)
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

(define/contract (exec chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list #\e #\x #\e #\c)
     (define end-position (add-position start (string->list "exec")))
     (lexer-result end-position
                   (list (position-token (token 'exec "exec"))
                         start
                         end-position)
                   (list))]
    [(list-rest #\e #\x #\e #\c more)
     (define end-position (add-position start (string->list "exec")))
     (define import/token (position-token (token 'exec "exec")
                                          start
                                          end-position))
     (define chars-of-keyword (take chars (string-length "exec")))
     (define remaining-chars (drop chars (string-length "exec")))
     (define filename/result (import-filename remaining-chars
                                              end-position))
     (lexer-result (lexer-result-end-position filename/result)
                   (cons import/token
                         (lexer-result-tokens filename/result))
                   (lexer-result-characters filename/result))]))

(define/contract (char-identifier? x)
  (char? . -> . boolean?)
  (match x
    [(or (? char-alphabetic?) (? char-numeric?) #\_ #\-)
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
    [(list-rest #\@ (? char-identifier?) ..1 _)
     (define ident-chars (read-identifier-chars (cdr chars)))
     (define token-content (token 'ENV-IDENTIFIER (list->string ident-chars)))
     (define end-position (add-position start (cons #\@ ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (define remaining-chars (drop chars (length (cons #\@ ident-chars))))
     (match remaining-chars
       [(list-rest (? char-whitespace?) ..1 #\w #\i #\t #\h
                   (? char-whitespace?) ..1 #\f #\a #\l #\l #\b #\a #\c #\k
                   (? char-whitespace?) _)
        (define with/result (consume-keyword "with" remaining-chars end-position))
        (define fallback/result (consume-keyword "fallback"
                                                 (lexer-result-characters with/result)
                                                 (lexer-result-end-position with/result)))
        (define value/result (lex-jsonish-stuff (lexer-result-characters fallback/result)
                                                (lexer-result-end-position fallback/result)))
        (lexer-result (lexer-result-end-position value/result)
                      (append (list id/token)
                              (lexer-result-tokens with/result)
                              (lexer-result-tokens fallback/result)
                              (lexer-result-tokens value/result))
                      (lexer-result-characters value/result))]
       [else
        (lexer-result end-position
                      (list id/token)
                      remaining-chars)])]))

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
     (define token-content (token 'REQUEST-HEADER-IDENTIFIER (list->string ident-chars)))
     (define end-position (add-position start (cons #\^ ident-chars)))
     (define id/token (position-token token-content
                                      start
                                      end-position))
     (lexer-result end-position
                   (list id/token)
                   (drop chars (add1 (length ident-chars))))]))

(define/contract (response-header-identifier chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(or (list (? header-char?) ..1 #\^)
         (list-rest (? header-char?) ..1 #\^ _))
     (define ident-chars (takef chars (lambda (c) (not (char=? c #\^)))))
     (define token-content (token 'RESPONSE-HEADER-IDENTIFIER (list->string ident-chars)))
     (define end-position (add-position start (append ident-chars (list #\^))))
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
     (define token-content (token 'PARAMETER (list->string ident-chars)))
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
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons (or #\{ #\? #\* #\,) _)
     (define c (car chars))
     (define new-position (add-position start (car chars)))
     (define template/token (position-token (token (string->symbol (~a c))
                                                   (~a c))
                                            start
                                            new-position))
     (define more/result (uri-template:template (cdr chars)
                                                new-position))
     (struct-copy lexer-result
                  more/result
                  [tokens (cons template/token (lexer-result-tokens more/result))])]
    [(cons #\} _)
     (define new-position (add-position start #\}))
     (define t (position-token (token (string->symbol "}") "}")
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
  (match chars
    [(list-rest #\" (not #\") ... #\" _)
     (define idx-of-double-quote (index-of (cdr chars) #\" char=?))
     (define string-chars (take (cdr chars) idx-of-double-quote))
     (define new-position (add-position start (cons #\" string-chars)))
     (define string/token (position-token (token 'JSON-STRING (list->string string-chars))
                                          start
                                          new-position))
     (define remaining-chars (drop chars (+ idx-of-double-quote 2)))
     (lexer-result new-position
                   (list string/token)
                   remaining-chars)]))

(define/contract (read-integer-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (cons (car cs)
           (read-integer-chars (cdr cs)))]
    [else
     (list)]))

(define/contract (read-number-chars cs)
  ((listof char?) . -> . (listof char?))
  (match cs
    [(or (list #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1)
         (list-rest #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 _))
     (cons #\- (read-integer-chars (cdr cs)))]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (read-integer-chars cs)]
    [else
     (list)]))

(define/contract (number chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define digits (read-number-chars chars))
  (when (empty? chars)
    (error "Failed to read any number characters!"))
  (define remaining-chars (drop chars (length digits)))
  (define end-position (add-position start digits))
  (match remaining-chars
    [(list)
     (lexer-result end-position
                   (list (position-token (token 'NUMBER (string->number (list->string digits)))
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
       (define number/token (position-token (token 'NUMBER (string->number n))
                                            start
                                            end-pos-after-dot))
       (lexer-result end-pos-after-dot
                     (list number/token)
                     (drop chars (length (append digits (list #\.) after-decimal-digits)))))]
    [else
     (define number/token (position-token (token 'NUMBER (string->number (list->string digits)))
                                          start
                                          end-position))
     (lexer-result end-position
                   (list number/token)
                   (drop chars (length digits)))]))

(define/contract (eat-whitespace chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define (consume cs)
    (match cs
      [(list)
       (list)]
      [(cons (? char-whitespace? c) _)
       (cons c (consume (cdr cs)))]
      [else
       (list)]))
  (define consumed (consume chars))
  (define remaining-chars (drop chars (length consumed)))
  (define new-position (add-position start consumed))
  (lexer-result new-position
                (list)
                remaining-chars))

(define/contract (lex-json-array-items chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons #\# _)
     (define comment/result (comment chars start))
     (define tail/result (lex-json-array-items (lexer-result-characters comment/result)
                                               (lexer-result-end-position comment/result)))
     (struct-copy lexer-result
                  tail/result
                  [tokens (append (lexer-result-tokens comment/result)
                                  (lexer-result-tokens tail/result))])]
    [(cons #\] _)
     (define end-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol "]") "]")
                               start
                               end-position))
     (lexer-result end-position
                   (list t)
                   (cdr chars))]
    [(cons #\, _)
     (define after-comma-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol ",") ",")
                               start
                               after-comma-position))
     (define next-item/result (lex-jsonish-stuff (cdr chars)
                                            after-comma-position))
     (define tail/result (lex-json-array-items (lexer-result-characters next-item/result)
                                               (lexer-result-end-position next-item/result)))
     (struct-copy lexer-result
                  tail/result
                  [tokens (append (list t)
                                  (lexer-result-tokens next-item/result)
                                  (lexer-result-tokens tail/result))])]
    [(cons _ _)
     (define item/result (lex-jsonish-stuff chars start))
     (define tail/result (lex-json-array-items (lexer-result-characters item/result)
                                               (lexer-result-end-position item/result)))
     (struct-copy lexer-result
                  tail/result
                  [tokens (append (lexer-result-tokens item/result)
                                  (lexer-result-tokens tail/result))])]))

(define/contract (lex-json-array chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (lex-json-array (cdr chars)
                     (add-position start (car chars)))]
    [(cons #\[ _)
     (define after-opening-token/position (add-position start (car chars)))
     (define opening-token (position-token (token (string->symbol "[") "[")
                                           start
                                           after-opening-token/position))
     (define items/result (lex-json-array-items (cdr chars)
                                                after-opening-token/position))
     (struct-copy lexer-result
                  items/result
                  [tokens (cons opening-token
                                (lexer-result-tokens items/result))])]))

(define/contract (lex-json-object-items chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (lex-json-object-items (cdr chars)
                            (add-position start (car chars)))]
    [(cons #\# _)
     (define comment/result (comment chars start))
     (define tail/result (lex-json-object-items (lexer-result-characters comment/result)
                                                (lexer-result-end-position comment/result)))
     (struct-copy lexer-result
                  tail/result
                  [tokens (append (lexer-result-tokens comment/result)
                                  (lexer-result-tokens tail/result))])]
    [(cons #\, _)
     (define after-comma-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol ",") ",")
                               start
                               after-comma-position))
     (define next-item/result (lex-json-object-items (cdr chars)
                                                     after-comma-position))
     (struct-copy lexer-result
                  next-item/result
                  [tokens (append (list t)
                                  (lexer-result-tokens next-item/result))])]
    [(cons #\} _)
     (define end-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol "}") "}")
                               start
                               end-position))
     (lexer-result end-position
                   (list t)
                   (cdr chars))]
    [(cons #\" _)
     (define property/result (json-string chars start))
     (define remaining-chars (lexer-result-characters property/result))
     (match remaining-chars
       [(list-rest (? char-whitespace?) ... #\: _)
        (define whitespace-chars (takef remaining-chars char-whitespace?))
        (define after-whitespace-chars (dropf remaining-chars char-whitespace?))
        (define colon-position (add-position (lexer-result-end-position property/result)
                                             whitespace-chars))
        (define after-colon-position (add-position colon-position #\:))
        (define colon-token (position-token (token (string->symbol ":") ":")
                                            colon-position
                                            after-colon-position))
        (define value/result (lex-jsonish-stuff (cdr after-whitespace-chars)
                                                after-colon-position))
        (define tail/result (lex-json-object-items (lexer-result-characters value/result)
                                                   (lexer-result-end-position value/result)))
        (struct-copy lexer-result
                     tail/result
                     [tokens (append (lexer-result-tokens property/result)
                                     (list colon-token)
                                     (lexer-result-tokens value/result)
                                     (lexer-result-tokens tail/result))])])]))

(define/contract (lex-json-object chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (lex-json-object (cdr chars)
                      (add-position start (car chars)))]
    [(cons #\{ _)
     (define new-position (add-position start (car chars)))
     (define opening-token (position-token (token (string->symbol "{") "{")
                                           start
                                           new-position))
     (define items/result (lex-json-object-items (cdr chars)
                                                 new-position))
     (struct-copy lexer-result
                  items/result
                  [tokens (cons opening-token (lexer-result-tokens items/result))])]))

(define/contract (lex-jsonish-stuff chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (lexer-result start
                   (list)
                   (list))]
    [(cons #\# _)
     (define comment/result (comment chars start))
     (define tail/result (lex-jsonish-stuff (lexer-result-characters comment/result)
                                            (lexer-result-end-position comment/result)))
     (struct-copy lexer-result
                  tail/result
                  [tokens (append (lexer-result-tokens comment/result)
                                  (lexer-result-tokens tail/result))])]
    [(cons (? char-whitespace? c) _)
     (lex-jsonish-stuff (cdr chars)
                        (add-position start c))]
    [(cons #\$ _)
     (identifier chars start)]
    [(cons #\@ _)
     (environment-identifier chars start)]
    [(cons #\{ _)
     (lex-json-object chars start)]
    [(cons #\[ _)
     (lex-json-array chars start)]
    [(cons #\" _)
     (json-string chars start)]
    [(list-rest #\n #\u #\l #\l _)
     (consume-keyword "null" chars start)]
    [(list-rest #\t #\r #\u #\e _)
     (consume-keyword "true" chars start)]
    [(list-rest #\f #\a #\l #\s #\e _)
     (consume-keyword "false" chars start)]
    [(list #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1)
     (number chars start)]
    [(list-rest #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 _)
     (number chars start)]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (number chars start)]
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
  (define method/token (position-token (token 'HTTP-METHOD (list->string method-chars))
                                       start
                                       position-after-method))
  (define remaining-characters (drop chars (length method-chars)))
  (lexer-result position-after-method
                (list method/token)
                remaining-characters))

(define/contract (after-http-method-payload chars start)
  ((listof char?) position? . -> . lexer-result?)
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

(define/contract (function-name chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (function-name (cdr chars)
                    (add-position start (car chars)))]
    [(list-rest (? char-identifier?) ..1 _)
     (define function-chars (takef chars char-identifier?))
     (define end-position (add-position start function-chars))
     (define t (position-token (token 'FUNCTION-NAME (list->string function-chars))
                               start
                               end-position))
     (lexer-result end-position
                   (list t)
                   (drop chars (length function-chars)))]))

(define/contract (function chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (function (cdr chars)
               (add-position start (car chars)))]
    [(list #\f #\u #\n #\c #\t #\i #\o #\n)
     (consume-keyword "function" chars start)]
    [(list-rest #\f #\u #\n #\c #\t #\i #\o #\n (? char-whitespace?) _)
     (define keyword-result (consume-keyword "function" chars start))
     (define remaining-chars (lexer-result-characters keyword-result))
     (define after-keyword-position (lexer-result-end-position keyword-result))
     (define function-name-result (function-name remaining-chars after-keyword-position))
     (lexer-result (lexer-result-end-position function-name-result)
                   (append (lexer-result-tokens keyword-result)
                           (lexer-result-tokens function-name-result))
                   (lexer-result-characters function-name-result))]))

(define/contract (call chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(cons (? char-whitespace?) _)
     (function (cdr chars)
               (add-position start (car chars)))]
    [(list #\c #\a #\l #\l)
     (consume-keyword "call" chars start)]
    [(list-rest #\c #\a #\l #\l (? char-whitespace?) _)
     (define keyword-result (consume-keyword "call" chars start))
     (define remaining-chars (lexer-result-characters keyword-result))
     (define after-keyword-position (lexer-result-end-position keyword-result))
     (define function-name-result (function-name remaining-chars after-keyword-position))
     (lexer-result (lexer-result-end-position function-name-result)
                   (append (lexer-result-tokens keyword-result)
                           (lexer-result-tokens function-name-result))
                   (lexer-result-characters function-name-result))]))

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
  (non-empty-string? (listof char?) position? . -> . lexer-result?)
  (match chars
    [(list)
     (error "Cannot consume keyword \"~a\": no more characters left!" keyword)]
    [(cons (? char-whitespace?) _)
     (consume-keyword keyword (cdr chars) (add-position start (car chars)))]
    [else
     (define string-chars (string->list keyword))
     (define initial-segment (take chars (length string-chars)))
     (unless (= (length initial-segment) (length string-chars))
       (error (format "Too few characters available! Cannot lex \"~a\" because all we have are ~a"
                      keyword
                      chars)))
     (define end-position (add-position start initial-segment))
     (define keyword/token (position-token (token (string->symbol keyword)
                                                  keyword)
                                           start
                                           end-position))
     (lexer-result end-position
                   (list keyword/token)
                   (drop chars (length initial-segment)))]))

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

(define (bang-char? x)
  (char=? x #\!))

(define/contract (bang chars start)
  ((listof char?) position? . -> . lexer-result?)
  (define bang-chars (takef chars bang-char?))
  (define num-chars (length bang-chars))
  (define remainder (drop chars num-chars))
  (define end-position (add-position start bang-chars))
  (lexer-result end-position
                (list (position-token (token 'BANG num-chars)
                                      start
                                      end-position))
                remainder))

(define/contract (assignment chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\: #\= _)
     (define end-position (add-position start (take chars 2)))
     (lexer-result end-position
                   (list (position-token (token (string->symbol ":=") ":=")
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
     (define parameter-token (position-token (token 'PARAMETER (list->string parameter-chars))
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

(define (timeout-parameter chars start)
  ((listof char?) position? . -> . lexer-result?)
  (match chars
    [(list-rest #\% #\t #\i #\m #\e #\o #\u #\t
                (? char-whitespace?) ..1
                _)
     (define parameter-chars (take (cdr chars) (string-length "timeout")))
     (define remaining-chars (drop chars (string-length "%timeout")))
     (define new-position (add-position start (take chars (string-length "%timeout"))))
     (define parameter-token (position-token (token 'PARAMETER (list->string parameter-chars))
                                             start
                                             new-position))
     (lexer-result new-position
                   (list parameter-token)
                   remaining-chars)]))

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
    [(list-rest #\e #\x #\e #\c (? char-whitespace?) _)
     (define result (exec chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\$ _)
     (define result (identifier chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\, _)
     (define c (car chars))
     (define new-position (add-position start c))
     (define t (position-token (token (string->symbol (~a c)) c)
                               start
                               new-position))
     (cons t (initial (cdr chars)
                      new-position))]
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
    [(or (list (? header-char?) ..1 #\^)
         (list-rest (? header-char?) ..1 #\^ _))
     (define result (response-header-identifier chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\% #\b #\a #\s #\e _)
     (define result (base-parameter chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\% #\t #\i #\m #\e #\o #\u #\t _)
     (define result (timeout-parameter chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\m #\a #\t #\c #\h #\e #\s)
         (list-rest #\m #\a #\t #\c #\h #\e #\s (? char-whitespace?) _))
     (define result (consume-keyword "matches" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons #\# _)
     (define result (comment chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\( #\)) _)
     (define s (~a (car chars)))
     (define new-position (add-position start (car chars)))
     (define t (position-token (token (string->symbol s) s)
                               start
                               new-position))
     (cons t (initial (cdr chars)
                      new-position))]
    [(cons #\/ _)
     (define result (json-pointer chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1)
         (list-rest #\- (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ..1 _))
     (define result (number chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) _)
     (define result (number chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\f #\a #\l #\s #\e _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\t #\r #\u #\e _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\n #\u #\l #\l _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\: #\= _)
     (define result (assignment chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\! ..1)
         (list-rest #\! ..1 (? char-whitespace?) _))
     (define result (bang chars start))
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
    [(cons (or #\" #\{ #\[) _)
     (define result (lex-jsonish-stuff chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\< #\=)
         (list-rest #\< #\= (? char-whitespace?) _))
     (define result (consume-keyword "<=" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\> #\=)
         (list-rest #\> #\= (? char-whitespace?) _))
     (define result (consume-keyword ">=" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(cons (or #\= #\< #\> #\+ #\- #\*) _)
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
    [(or (list #\e #\m #\p #\t #\y)
         (list-rest #\e #\m #\p #\t #\y _))
     (define result (consume-keyword "empty" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\e #\v #\e #\n)
         (list-rest #\e #\v #\e #\n _))
     (define result (consume-keyword "even" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\o #\d #\d)
         (list-rest #\o #\d #\d _))
     (define result (consume-keyword "odd" chars start))
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
    [(or (list #\a)
         (list-rest #\a (? char-whitespace?) _))
     (define result (consume-keyword "a" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\a #\n)
         (list-rest #\a #\n (? char-whitespace?) _))
     (define result (consume-keyword "an" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\a #\n #\d)
         (list-rest #\a #\n #\d (? char-whitespace?) _))
     (define result (consume-keyword "and" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\a #\t #\i #\s #\f #\y)
         (list-rest #\s #\a #\t #\i #\s #\f #\y (? char-whitespace?) _))
     (define result (consume-keyword "satisfy" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\a #\t #\i #\s #\f #\i #\e #\s)
         (list-rest #\s #\a #\t #\i #\s #\f #\i #\e #\s (? char-whitespace?) _))
     (define result (consume-keyword "satisfies" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\s #\c #\h #\e #\m #\a _)
     (define result (consume-keyword "schema" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\i #\n #\t #\e #\g #\e #\r)
         (list-rest #\i #\n #\t #\e #\g #\e #\r _))
     (define result (consume-keyword "integer" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\a #\b #\s #\e #\n #\t)
         (list-rest #\a #\b #\s #\e #\n #\t _))
     (define result (consume-keyword "absent" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\t #\a #\r #\t #\s)
         (list-rest #\s #\t #\a #\r #\t #\s _))
     (define result (consume-keyword "starts" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\e #\n #\d #\s)
         (list-rest #\e #\n #\d #\s _))
     (define result (consume-keyword "ends" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\p #\r #\e #\s #\e #\n #\t)
         (list-rest #\p #\r #\e #\s #\e #\n #\t _))
     (define result (consume-keyword "present" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\a #\r #\r #\a #\y)
         (list-rest #\a #\r #\r #\a #\y _))
     (define result (consume-keyword "array" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\p #\o #\s #\i #\t #\i #\v #\e)
         (list-rest #\p #\o #\s #\i #\t #\i #\v #\e _))
     (define result (consume-keyword "positive" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\e #\q #\u #\a #\l #\s)
         (list-rest #\e #\q #\u #\a #\l #\s _))
     (define result (consume-keyword "equals" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\n #\u #\m #\b #\e #\r)
         (list-rest #\n #\u #\m #\b #\e #\r _))
     (define result (consume-keyword "number" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\o #\b #\j #\e #\c #\t)
         (list-rest #\o #\b #\j #\e #\c #\t _))
     (define result (consume-keyword "object" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\b #\o #\o #\l #\e #\a #\n)
         (list-rest #\b #\o #\o #\l #\e #\a #\n _))
     (define result (consume-keyword "boolean" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\t #\r #\i #\n #\g)
         (list-rest #\s #\t #\r #\i #\n #\g _))
     (define result (consume-keyword "string" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\a #\r #\g #\u #\m #\e #\n #\t #\s)
         (list-rest #\a #\r #\g #\u #\m #\e #\n #\t #\s (? char-whitespace?) _))
     (define result (consume-keyword "arguments" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\n #\e #\g #\a #\t #\i #\v #\e)
         (list-rest #\n #\e #\g #\a #\t #\i #\v #\e _))
     (define result (consume-keyword "negative" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\i #\n _)
     (define result (in chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\e #\x #\p #\l #\o #\d #\e #\d)
         (list-rest #\e #\x #\p #\l #\o #\d #\e #\d _))
     (define result (consume-keyword "exploded" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\w #\i #\t #\h _)
     (define result (consume-keyword "with" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\h #\e #\a #\d #\e #\r #\s _)
     (define result (consume-keyword "headers" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest #\e #\c #\h #\o _)
     (define result (consume-keyword "echo" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\u #\n #\s #\e #\t)
         (list-rest #\u #\n #\s #\e #\t _))
     (define result (consume-keyword "unset" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\t #\i #\m #\e #\s)
         (list-rest #\t #\i #\m #\e #\s _))
     (define result (consume-keyword "times" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\o #\u #\t)
         (list-rest #\o #\u #\t _))
     (define result (consume-keyword "out" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\l #\e #\e #\p)
         (list-rest #\s #\l #\e #\e #\p _))
     (define result (consume-keyword "sleep" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\e #\c #\o #\n #\d #\s)
         (list-rest #\s #\e #\c #\o #\n #\d #\s _))
     (define result (consume-keyword "seconds" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\s #\e #\c #\o #\n #\d)
         (list-rest #\s #\e #\c #\o #\n #\d _))
     (define result (consume-keyword "second" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\m #\i #\n #\u #\t #\e)
         (list-rest #\m #\i #\n #\u #\t #\e _))
     (define result (consume-keyword "minute" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\m #\i #\n #\u #\t #\e #\s)
         (list-rest #\m #\i #\n #\u #\t #\e #\s _))
     (define result (consume-keyword "minutes" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\f #\u #\n #\c #\t #\i #\o #\n)
         (list-rest #\f #\u #\n #\c #\t #\i #\o #\n _))
     (define result (function chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\b #\e #\g #\i #\n)
         (list-rest #\b #\e #\g #\i #\n _))
     (define result (consume-keyword "begin" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\e #\n #\d)
         (list-rest #\e #\n #\d (not #\s) _))
     (define result (consume-keyword "end" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\c #\a #\l #\l)
         (list-rest #\c #\a #\l #\l _))
     (define result (call chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(or (list #\r #\e #\t #\u #\r #\n)
         (list-rest #\r #\e #\t #\u #\r #\n (? char-whitespace?) _))
     (define result (consume-keyword "return" chars start))
     (append (lexer-result-tokens result)
             (initial (lexer-result-characters result)
                      (lexer-result-end-position result)))]
    [(list-rest (? char-identifier?) ..1 (? char-whitespace?) ... #\( _)
     (define result (function-name chars start))
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

(module+ test
  (let ([program #<<RIPOSTE
import foo.rip
$a := 4.567
RIPOSTE
                 ])
    (check-tokenize program
                    (list
                     (position-token
                      (token-struct 'import #f #f #f #f #f #f)
                      (position 1 1 0)
                      (position 7 1 6))
                     (position-token
                      (token-struct 'FILENAME "foo.rip" #f #f #f #f #f)
                      (position 8 1 7)
                      (position 15 1 14))
                     (position-token
                      (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
                      (position 16 2 0)
                      (position 18 2 2))
                     (position-token
                      (token-struct ':= ":=" #f #f #f #f #f)
                      (position 19 2 3)
                      (position 21 2 5))
                     (position-token
                      (token-struct 'NUMBER 4567/1000 #f #f #f #f #f)
                      (position 22 2 6)
                      (position 27 2 11))))))

(module+ test
  (check-tokenize
   "$a := { \"foo\": true, \"bar\": $bar } $b := false"
   (list
   (position-token
    (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
    (position 1 1 0)
    (position 3 1 2))
   (position-token
    (token-struct ':= ":=" #f #f #f #f #f)
    (position 4 1 3)
    (position 6 1 5))
   (position-token
    (token-struct '|{| "{" #f #f #f #f #f)
    (position 7 1 6)
    (position 8 1 7))
   (position-token
    (token-struct 'JSON-STRING "foo" #f #f #f #f #f)
    (position 9 1 8)
    (position 13 1 12))
   (position-token
    (token-struct ': ":" #f #f #f #f #f)
    (position 13 1 12)
    (position 14 1 13))
   (position-token
    (token-struct 'true "true" #f #f #f #f #f)
    (position 15 1 14)
    (position 19 1 18))
   (position-token
    (token-struct '|,| "," #f #f #f #f #f)
    (position 19 1 18)
    (position 20 1 19))
   (position-token
    (token-struct 'JSON-STRING "bar" #f #f #f #f #f)
    (position 21 1 20)
    (position 25 1 24))
   (position-token
    (token-struct ': ":" #f #f #f #f #f)
    (position 25 1 24)
    (position 26 1 25))
   (position-token
    (token-struct 'IDENTIFIER "bar" #f #f #f #f #f)
    (position 27 1 26)
    (position 31 1 30))
   (position-token
    (token-struct '|}| "}" #f #f #f #f #f)
    (position 32 1 31)
    (position 33 1 32))
   (position-token
    (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
    (position 34 1 33)
    (position 36 1 35))
   (position-token
    (token-struct ':= ":=" #f #f #f #f #f)
    (position 37 1 36)
    (position 39 1 38))
   (position-token
    (token-struct 'false "false" #f #f #f #f #f)
    (position 40 1 39)
    (position 45 1 44)))))

(module+ test
  (check-tokenize
   "$a := [ 4 ]"
   (list
   (position-token
    (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
    (position 1 1 0)
    (position 3 1 2))
   (position-token
    (token-struct ':= ":=" #f #f #f #f #f)
    (position 4 1 3)
    (position 6 1 5))
   (position-token
    (token-struct '|[| "[" #f #f #f #f #f)
    (position 7 1 6)
    (position 8 1 7))
   (position-token
    (token-struct 'NUMBER 4 #f #f #f #f #f)
    (position 9 1 8)
    (position 10 1 9))
   (position-token
    (token-struct '|]| "]" #f #f #f #f #f)
    (position 11 1 10)
    (position 12 1 11)))))

(module+ test
  (check-tokenize
   "%base := http://foo.example.com"
   (list
   (position-token
    (token-struct 'PARAMETER "base" #f #f #f #f #f)
    (position 1 1 0)
    (position 6 1 5))
   (position-token
    (token-struct ':= #f #f #f #f #f #f)
    (position 7 1 6)
    (position 9 1 8))
   (position-token
    (token-struct 'URI-TEMPLATE-LITERAL "http://foo.example.com" #f #f #f #f #f)
    (position 10 1 9)
    (position 32 1 31)))))

(module+ test
  (check-tokenize
   "%timeout := 45"
   (list
    (position-token
     (token-struct 'PARAMETER "timeout" #f #f #f #f #f)
     (position 1 1 0)
     (position 9 1 8))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 10 1 9)
     (position 12 1 11))
    (position-token
     (token-struct 'NUMBER 45 #f #f #f #f #f)
     (position 13 1 12)
     (position 15 1 14)))))

(module+ test
  (check-tokenize
   "unset ^Content-Type"
   (list
    (position-token
     (token-struct 'unset "unset" #f #f #f #f #f)
     (position 1 1 0)
     (position 6 1 5))
    (position-token
     (token-struct 'REQUEST-HEADER-IDENTIFIER "Content-Type" #f #f #f #f #f)
     (position 7 1 6)
     (position 20 1 19)))))

(module+ test
  (check-tokenize
   "POST $payload to api/flub with headers $heads responds with 2XX"
   (list
    (position-token
     (token-struct 'HTTP-METHOD "POST" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct 'IDENTIFIER "payload" #f #f #f #f #f)
     (position 6 1 5)
     (position 14 1 13))
    (position-token
     (token-struct 'to "to" #f #f #f #f #f)
     (position 15 1 14)
     (position 17 1 16))
    (position-token
     (token-struct 'URI-TEMPLATE-LITERAL "api/flub" #f #f #f #f #f)
     (position 18 1 17)
     (position 26 1 25))
    (position-token
     (token-struct 'with "with" #f #f #f #f #f)
     (position 27 1 26)
     (position 31 1 30))
    (position-token
     (token-struct 'headers "headers" #f #f #f #f #f)
     (position 32 1 31)
     (position 39 1 38))
    (position-token
     (token-struct 'IDENTIFIER "heads" #f #f #f #f #f)
     (position 40 1 39)
     (position 46 1 45))
    (position-token
     (token-struct 'responds #f #f #f #f #f #f)
     (position 47 1 46)
     (position 55 1 54))
    (position-token
     (token-struct 'with #f #f #f #f #f #f)
     (position 56 1 55)
     (position 60 1 59))
    (position-token
     (token-struct 'HTTP-STATUS-CODE "2XX" #f #f #f #f #f)
     (position 61 1 60)
     (position 64 1 63)))))

(module+ test
  (check-tokenize
   "$foo is integer"
   (list
    (position-token
     (token-struct 'IDENTIFIER "foo" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7))
    (position-token
     (token-struct 'integer "integer" #f #f #f #f #f)
     (position 9 1 8)
     (position 16 1 15)))))

(module+ test
  (check-tokenize
   "$bar is not array"
   (list
    (position-token
     (token-struct 'IDENTIFIER "bar" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7))
    (position-token
     (token-struct 'not "not" #f #f #f #f #f)
     (position 9 1 8)
     (position 12 1 11))
    (position-token
     (token-struct 'array "array" #f #f #f #f #f)
     (position 13 1 12)
     (position 18 1 17)))))

(module+ test
  (check-tokenize
   "/foo is positive integer"
   (list
    (position-token
     (token-struct 'JSON-POINTER "/foo" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7))
    (position-token
     (token-struct 'positive "positive" #f #f #f #f #f)
     (position 9 1 8)
     (position 17 1 16))
    (position-token
     (token-struct 'integer "integer" #f #f #f #f #f)
     (position 18 1 17)
     (position 25 1 24)))))

(module+ test
  (check-tokenize
   "/-2 is negative"
   (list
    (position-token
     (token-struct 'JSON-POINTER "/-2" #f #f #f #f #f)
     (position 1 1 0)
     (position 4 1 3))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 5 1 4)
     (position 7 1 6))
    (position-token
     (token-struct 'negative "negative" #f #f #f #f #f)
     (position 8 1 7)
     (position 16 1 15)))))

(module+ test
  (check-tokenize
   "(-2) is negative"
   (list
    (position-token
     (token-struct '|(| "(" #f #f #f #f #f)
     (position 1 1 0)
     (position 2 1 1))
    (position-token
     (token-struct 'NUMBER -2 #f #f #f #f #f)
     (position 2 1 1)
     (position 4 1 3))
    (position-token
     (token-struct '|)| ")" #f #f #f #f #f)
     (position 4 1 3)
     (position 5 1 4))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7))
    (position-token
     (token-struct 'negative "negative" #f #f #f #f #f)
     (position 9 1 8)
     (position 17 1 16)))))

(module+ test
  (check-tokenize
   "sleep 5 seconds"
   (list
    (position-token
     (token-struct 'sleep "sleep" #f #f #f #f #f)
     (position 1 1 0)
     (position 6 1 5))
    (position-token
     (token-struct 'NUMBER 5 #f #f #f #f #f)
     (position 7 1 6)
     (position 8 1 7))
    (position-token
     (token-struct 'seconds "seconds" #f #f #f #f #f)
     (position 9 1 8)
     (position 16 1 15)))))

(module+ test
  (check-tokenize
   "sleep 1 minute"
   (list
   (position-token
    (token-struct 'sleep "sleep" #f #f #f #f #f)
    (position 1 1 0)
    (position 6 1 5))
   (position-token
    (token-struct 'NUMBER 1 #f #f #f #f #f)
    (position 7 1 6)
    (position 8 1 7))
   (position-token
    (token-struct 'minute "minute" #f #f #f #f #f)
    (position 9 1 8)
    (position 15 1 14)))))

(module+ test
  (check-tokenize
   "$foo := /bar (negative integer)"
   (list
    (position-token
     (token-struct 'IDENTIFIER "foo" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7))
    (position-token
     (token-struct 'JSON-POINTER "/bar" #f #f #f #f #f)
     (position 9 1 8)
     (position 13 1 12))
    (position-token
     (token-struct '|(| "(" #f #f #f #f #f)
     (position 14 1 13)
     (position 15 1 14))
    (position-token
     (token-struct 'negative "negative" #f #f #f #f #f)
     (position 15 1 14)
     (position 23 1 22))
    (position-token
     (token-struct 'integer "integer" #f #f #f #f #f)
     (position 24 1 23)
     (position 31 1 30))
    (position-token
     (token-struct '|)| ")" #f #f #f #f #f)
     (position 31 1 30)
     (position 32 1 31)))))

(module+ test
  (check-tokenize
   "$x is number"
   (list
    (position-token
     (token-struct 'IDENTIFIER "x" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'number "number" #f #f #f #f #f)
     (position 7 1 6)
     (position 13 1 12)))))

(module+ test
  (check-tokenize
   "$x is not object"
   (list
    (position-token
     (token-struct 'IDENTIFIER "x" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'not "not" #f #f #f #f #f)
     (position 7 1 6)
     (position 10 1 9))
    (position-token
     (token-struct 'object "object" #f #f #f #f #f)
     (position 11 1 10)
     (position 17 1 16)))))

(module+ test
  (check-tokenize
   "$x is boolean"
   (list
    (position-token
     (token-struct 'IDENTIFIER "x" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'boolean "boolean" #f #f #f #f #f)
     (position 7 1 6)
     (position 14 1 13)))))

(module+ test
  (check-tokenize
   "$john is string"
   (list
    (position-token
     (token-struct 'IDENTIFIER "john" #f #f #f #f #f)
     (position 1 1 0)
     (position 6 1 5))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct 'string "string" #f #f #f #f #f)
     (position 10 1 9)
     (position 16 1 15)))))

(module+ test
  (check-tokenize
   "Content-Type^ is absent"
   (list
    (position-token
     (token-struct 'RESPONSE-HEADER-IDENTIFIER "Content-Type" #f #f #f #f #f)
     (position 1 1 0)
     (position 14 1 13))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 15 1 14)
     (position 17 1 16))
    (position-token
     (token-struct 'absent "absent" #f #f #f #f #f)
     (position 18 1 17)
     (position 24 1 23)))))

(module+ test
  (check-tokenize
   "Content-Type^ is present"
   (list
    (position-token
     (token-struct 'RESPONSE-HEADER-IDENTIFIER "Content-Type" #f #f #f #f #f)
     (position 1 1 0)
     (position 14 1 13))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 15 1 14)
     (position 17 1 16))
    (position-token
     (token-struct 'present "present" #f #f #f #f #f)
     (position 18 1 17)
     (position 25 1 24)))))

(module+ test
  (check-tokenize
   "Content-Type^ starts with \"whatever\""
   (list
    (position-token
     (token-struct 'RESPONSE-HEADER-IDENTIFIER "Content-Type" #f #f #f #f #f)
     (position 1 1 0)
     (position 14 1 13))
    (position-token
     (token-struct 'starts "starts" #f #f #f #f #f)
     (position 15 1 14)
     (position 21 1 20))
    (position-token
     (token-struct 'with "with" #f #f #f #f #f)
     (position 22 1 21)
     (position 26 1 25))
    (position-token
     (token-struct 'JSON-STRING "whatever" #f #f #f #f #f)
     (position 27 1 26)
     (position 36 1 35)))))

(module+ test
  (check-tokenize
   "Content-Type^ ends with \"utf-8\""
   (list
    (position-token
     (token-struct 'RESPONSE-HEADER-IDENTIFIER "Content-Type" #f #f #f #f #f)
     (position 1 1 0)
     (position 14 1 13))
    (position-token
     (token-struct 'ends "ends" #f #f #f #f #f)
     (position 15 1 14)
     (position 19 1 18))
    (position-token
     (token-struct 'with "with" #f #f #f #f #f)
     (position 20 1 19)
     (position 24 1 23))
    (position-token
     (token-struct 'JSON-STRING "utf-8" #f #f #f #f #f)
     (position 25 1 24)
     (position 31 1 30)))))

(module+ test
  (check-tokenize
   "$a is even"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'even "even" #f #f #f #f #f)
     (position 7 1 6)
     (position 11 1 10)))))

(module+ test
  (check-tokenize
   "$a is odd"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'odd "odd" #f #f #f #f #f)
     (position 7 1 6)
     (position 10 1 9)))))

(module+ test
  (check-tokenize
   "$a is an integer"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'an "an" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct 'integer "integer" #f #f #f #f #f)
     (position 10 1 9)
     (position 17 1 16)))))

(module+ test
  (check-tokenize
   "$s is a string"
   (list
    (position-token
     (token-struct 'IDENTIFIER "s" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'is "is" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'a "a" #f #f #f #f #f)
     (position 7 1 6)
     (position 8 1 7))
    (position-token
     (token-struct 'string "string" #f #f #f #f #f)
     (position 9 1 8)
     (position 15 1 14)))))

(module+ test
  (check-tokenize
   "$a < $b"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct '< "<" #f #f #f #f #f)
     (position 4 1 3)
     (position 5 1 4))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7)))))

(module+ test
  (check-tokenize
   "$a <= $b"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct '<= "<=" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8)))))

(module+ test
  (check-tokenize
   "$a > $b"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct '> ">" #f #f #f #f #f)
     (position 4 1 3)
     (position 5 1 4))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7)))))

(module+ test
  (check-tokenize
   "$a >= $b"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct '>= ">=" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8)))))

(module+ test
  (check-tokenize
   "GET /foo does not satisfy schema { \"type\": \"array\" }"
   (list
   (position-token
    (token-struct 'HTTP-METHOD "GET" #f #f #f #f #f)
    (position 1 1 0)
    (position 4 1 3))
   (position-token
    (token-struct 'URI-TEMPLATE-LITERAL "/foo" #f #f #f #f #f)
    (position 5 1 4)
    (position 9 1 8))
   (position-token
    (token-struct 'does "does" #f #f #f #f #f)
    (position 10 1 9)
    (position 14 1 13))
   (position-token
    (token-struct 'not "not" #f #f #f #f #f)
    (position 15 1 14)
    (position 18 1 17))
   (position-token
    (token-struct 'satisfy "satisfy" #f #f #f #f #f)
    (position 19 1 18)
    (position 26 1 25))
   (position-token
    (token-struct 'schema "schema" #f #f #f #f #f)
    (position 27 1 26)
    (position 33 1 32))
   (position-token
    (token-struct '|{| "{" #f #f #f #f #f)
    (position 34 1 33)
    (position 35 1 34))
   (position-token
    (token-struct 'JSON-STRING "type" #f #f #f #f #f)
    (position 36 1 35)
    (position 41 1 40))
   (position-token
    (token-struct ': ":" #f #f #f #f #f)
    (position 41 1 40)
    (position 42 1 41))
   (position-token
    (token-struct 'JSON-STRING "array" #f #f #f #f #f)
    (position 43 1 42)
    (position 49 1 48))
   (position-token
    (token-struct '|}| "}" #f #f #f #f #f)
    (position 50 1 49)
    (position 51 1 50)))))

(module+ test
  (check-tokenize
   "$a := $b * $c"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct '* "*" #f #f #f #f #f)
     (position 10 1 9)
     (position 11 1 10))
    (position-token
     (token-struct 'IDENTIFIER "c" #f #f #f #f #f)
     (position 12 1 11)
     (position 14 1 13)))))

(module+ test
  (check-tokenize
   "POST { \"sum\": $a } to /jay responds with 201"
   (list
   (position-token
    (token-struct 'HTTP-METHOD "POST" #f #f #f #f #f)
    (position 1 1 0)
    (position 5 1 4))
   (position-token
    (token-struct '|{| "{" #f #f #f #f #f)
    (position 6 1 5)
    (position 7 1 6))
   (position-token
    (token-struct 'JSON-STRING "sum" #f #f #f #f #f)
    (position 8 1 7)
    (position 12 1 11))
   (position-token
    (token-struct ': ":" #f #f #f #f #f)
    (position 12 1 11)
    (position 13 1 12))
   (position-token
    (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
    (position 14 1 13)
    (position 16 1 15))
   (position-token
    (token-struct '|}| "}" #f #f #f #f #f)
    (position 17 1 16)
    (position 18 1 17))
   (position-token
    (token-struct 'to "to" #f #f #f #f #f)
    (position 19 1 18)
    (position 21 1 20))
   (position-token
    (token-struct 'URI-TEMPLATE-LITERAL "/jay" #f #f #f #f #f)
    (position 22 1 21)
    (position 26 1 25))
   (position-token
    (token-struct 'responds #f #f #f #f #f #f)
    (position 27 1 26)
    (position 35 1 34))
   (position-token
    (token-struct 'with #f #f #f #f #f #f)
    (position 36 1 35)
    (position 40 1 39))
   (position-token
    (token-struct 'HTTP-STATUS-CODE "201" #f #f #f #f #f)
    (position 41 1 40)
    (position 44 1 43)))))

(module+ test
  (check-tokenize
   "$a := $b - $c"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct '- "-" #f #f #f #f #f)
     (position 10 1 9)
     (position 11 1 10))
    (position-token
     (token-struct 'IDENTIFIER "c" #f #f #f #f #f)
     (position 12 1 11)
     (position 14 1 13)))))

(module+ test
  (check-tokenize
   "exec foo.sh"
   (list
   (position-token
    (token-struct 'exec "exec" #f #f #f #f #f)
    (position 1 1 0)
    (position 5 1 4))
   (position-token
    (token-struct 'FILENAME "foo.sh" #f #f #f #f #f)
    (position 6 1 5)
    (position 12 1 11)))))

(module+ test
  (check-tokenize
   "exec foo.sh with arguments [ \"--quiet\" ]"
   (list
   (position-token
    (token-struct 'exec "exec" #f #f #f #f #f)
    (position 1 1 0)
    (position 5 1 4))
   (position-token
    (token-struct 'FILENAME "foo.sh" #f #f #f #f #f)
    (position 6 1 5)
    (position 12 1 11))
   (position-token
    (token-struct 'with "with" #f #f #f #f #f)
    (position 13 1 12)
    (position 17 1 16))
   (position-token
    (token-struct 'arguments "arguments" #f #f #f #f #f)
    (position 18 1 17)
    (position 27 1 26))
   (position-token
    (token-struct '|[| "[" #f #f #f #f #f)
    (position 28 1 27)
    (position 29 1 28))
   (position-token
    (token-struct 'JSON-STRING "--quiet" #f #f #f #f #f)
    (position 30 1 29)
    (position 38 1 37))
   (position-token
    (token-struct '|]| "]" #f #f #f #f #f)
    (position 39 1 38)
    (position 40 1 39)))))

(module+ test
  (check-tokenize
   "exec foo.sh with arguments [ $a, $b ]"
   (list
   (position-token
    (token-struct 'exec "exec" #f #f #f #f #f)
    (position 1 1 0)
    (position 5 1 4))
   (position-token
    (token-struct 'FILENAME "foo.sh" #f #f #f #f #f)
    (position 6 1 5)
    (position 12 1 11))
   (position-token
    (token-struct 'with "with" #f #f #f #f #f)
    (position 13 1 12)
    (position 17 1 16))
   (position-token
    (token-struct 'arguments "arguments" #f #f #f #f #f)
    (position 18 1 17)
    (position 27 1 26))
   (position-token
    (token-struct '|[| "[" #f #f #f #f #f)
    (position 28 1 27)
    (position 29 1 28))
   (position-token
    (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
    (position 30 1 29)
    (position 32 1 31))
   (position-token
    (token-struct '|,| "," #f #f #f #f #f)
    (position 32 1 31)
    (position 33 1 32))
   (position-token
    (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
    (position 34 1 33)
    (position 36 1 35))
   (position-token
    (token-struct '|]| "]" #f #f #f #f #f)
    (position 37 1 36)
    (position 38 1 37)))))

(module+ test
  (check-tokenize
   "GET /foo/bar equals { \"wow!\": false }"
   (list
   (position-token
    (token-struct 'HTTP-METHOD "GET" #f #f #f #f #f)
    (position 1 1 0)
    (position 4 1 3))
   (position-token
    (token-struct 'URI-TEMPLATE-LITERAL "/foo/bar" #f #f #f #f #f)
    (position 5 1 4)
    (position 13 1 12))
   (position-token
    (token-struct 'equals "equals" #f #f #f #f #f)
    (position 14 1 13)
    (position 20 1 19))
   (position-token
    (token-struct '|{| "{" #f #f #f #f #f)
    (position 21 1 20)
    (position 22 1 21))
   (position-token
    (token-struct 'JSON-STRING "wow!" #f #f #f #f #f)
    (position 23 1 22)
    (position 28 1 27))
   (position-token
    (token-struct ': ":" #f #f #f #f #f)
    (position 28 1 27)
    (position 29 1 28))
   (position-token
    (token-struct 'false "false" #f #f #f #f #f)
    (position 30 1 29)
    (position 35 1 34))
   (position-token
    (token-struct '|}| "}" #f #f #f #f #f)
    (position 36 1 35)
    (position 37 1 36)))))

(module+ test
  (check-tokenize
   "! != !!"
   (list
    (position-token
     (token-struct 'BANG 1 #f #f #f #f #f)
     (position 1 1 0)
     (position 2 1 1))
    (position-token
     (token-struct '!= "!=" #f #f #f #f #f)
     (position 3 1 2)
     (position 5 1 4))
    (position-token
     (token-struct 'BANG 2 #f #f #f #f #f)
     (position 6 1 5)
     (position 8 1 7)))))

(module+ test
  (check-tokenize
   "!!! = !!!!"
   (list
    (position-token
     (token-struct 'BANG 3 #f #f #f #f #f)
     (position 1 1 0)
     (position 4 1 3))
    (position-token
     (token-struct '= "=" #f #f #f #f #f)
     (position 5 1 4)
     (position 6 1 5))
    (position-token
     (token-struct 'BANG 4 #f #f #f #f #f)
     (position 7 1 6)
     (position 11 1 10)))))

(module+ test
  (check-tokenize
   "%timeout := 15"
   (list
   (position-token
    (token-struct 'PARAMETER "timeout" #f #f #f #f #f)
    (position 1 1 0)
    (position 9 1 8))
   (position-token
    (token-struct ':= ":=" #f #f #f #f #f)
    (position 10 1 9)
    (position 12 1 11))
   (position-token
    (token-struct 'NUMBER 15 #f #f #f #f #f)
    (position 13 1 12)
    (position 15 1 14)))))

(module+ test
  (check-tokenize
   "GET foo/bar times out"
   (list
    (position-token
     (token-struct 'HTTP-METHOD "GET" #f #f #f #f #f)
     (position 1 1 0)
     (position 4 1 3))
    (position-token
     (token-struct 'URI-TEMPLATE-LITERAL "foo/bar" #f #f #f #f #f)
     (position 5 1 4)
     (position 12 1 11))
    (position-token
     (token-struct 'times "times" #f #f #f #f #f)
     (position 13 1 12)
     (position 18 1 17))
    (position-token
     (token-struct 'out "out" #f #f #f #f #f)
     (position 19 1 18)
     (position 22 1 21)))))

(module+ test
  (check-tokenize
   "$a := $b exploded with \"+\""
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 4 1 3)
     (position 6 1 5))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct 'exploded "exploded" #f #f #f #f #f)
     (position 10 1 9)
     (position 18 1 17))
    (position-token
     (token-struct 'with "with" #f #f #f #f #f)
     (position 19 1 18)
     (position 23 1 22))
    (position-token
     (token-struct 'JSON-STRING "+" #f #f #f #f #f)
     (position 24 1 23)
     (position 26 1 25)))))

(module+ test
  (check-tokenize
   "/foo = -6"
   (list
    (position-token
     (token-struct 'JSON-POINTER "/foo" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct '= "=" #f #f #f #f #f)
     (position 6 1 5)
     (position 7 1 6))
    (position-token
     (token-struct 'NUMBER -6 #f #f #f #f #f)
     (position 8 1 7)
     (position 10 1 9)))))

(module+ test
  (check-tokenize
   "$a matches { \"foo\": $bar }"
   (list
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 1 1 0)
     (position 3 1 2))
    (position-token
     (token-struct 'matches "matches" #f #f #f #f #f)
     (position 4 1 3)
     (position 11 1 10))
    (position-token
     (token-struct '|{| "{" #f #f #f #f #f)
     (position 12 1 11)
     (position 13 1 12))
    (position-token
     (token-struct 'JSON-STRING "foo" #f #f #f #f #f)
     (position 14 1 13)
     (position 18 1 17))
    (position-token
     (token-struct ': ":" #f #f #f #f #f)
     (position 18 1 17)
     (position 19 1 18))
    (position-token
     (token-struct 'IDENTIFIER "bar" #f #f #f #f #f)
     (position 20 1 19)
     (position 24 1 23))
    (position-token
     (token-struct '|}| "}" #f #f #f #f #f)
     (position 25 1 24)
     (position 26 1 25)))))

(module+ test
  (check-tokenize
   "function foo($a, $b) := begin GET x responds with 2XX return /foo end"
   (list
    (position-token
     (token-struct 'function "function" #f #f #f #f #f)
     (position 1 1 0)
     (position 9 1 8))
    (position-token
     (token-struct 'FUNCTION-NAME "foo" #f #f #f #f #f)
     (position 10 1 9)
     (position 13 1 12))
    (position-token
     (token-struct '|(| "(" #f #f #f #f #f)
     (position 13 1 12)
     (position 14 1 13))
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 14 1 13)
     (position 16 1 15))
    (position-token
     (token-struct '|,| #\, #f #f #f #f #f)
     (position 16 1 15)
     (position 17 1 16))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 18 1 17)
     (position 20 1 19))
    (position-token
     (token-struct '|)| ")" #f #f #f #f #f)
     (position 20 1 19)
     (position 21 1 20))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 22 1 21)
     (position 24 1 23))
    (position-token
     (token-struct 'begin "begin" #f #f #f #f #f)
     (position 25 1 24)
     (position 30 1 29))
    (position-token
     (token-struct 'HTTP-METHOD "GET" #f #f #f #f #f)
     (position 31 1 30)
     (position 34 1 33))
    (position-token
     (token-struct 'URI-TEMPLATE-LITERAL "x" #f #f #f #f #f)
     (position 35 1 34)
     (position 36 1 35))
    (position-token
     (token-struct 'responds #f #f #f #f #f #f)
     (position 37 1 36)
     (position 45 1 44))
    (position-token
     (token-struct 'with #f #f #f #f #f #f)
     (position 46 1 45)
     (position 50 1 49))
    (position-token
     (token-struct 'HTTP-STATUS-CODE "2XX" #f #f #f #f #f)
     (position 51 1 50)
     (position 54 1 53))
    (position-token
     (token-struct 'return "return" #f #f #f #f #f)
     (position 55 1 54)
     (position 61 1 60))
    (position-token
     (token-struct 'JSON-POINTER "/foo" #f #f #f #f #f)
     (position 62 1 61)
     (position 66 1 65))
    (position-token
     (token-struct 'end "end" #f #f #f #f #f)
     (position 67 1 66)
     (position 70 1 69)))))

(module+ test
  (check-tokenize
   "function foo($a, $b) := { \"foo\": true }"
   (list
    (position-token
     (token-struct 'function "function" #f #f #f #f #f)
     (position 1 1 0)
     (position 9 1 8))
    (position-token
     (token-struct 'FUNCTION-NAME "foo" #f #f #f #f #f)
     (position 10 1 9)
     (position 13 1 12))
    (position-token
     (token-struct '|(| "(" #f #f #f #f #f)
     (position 13 1 12)
     (position 14 1 13))
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 14 1 13)
     (position 16 1 15))
    (position-token
     (token-struct '|,| #\, #f #f #f #f #f)
     (position 16 1 15)
     (position 17 1 16))
    (position-token
     (token-struct 'IDENTIFIER "b" #f #f #f #f #f)
     (position 18 1 17)
     (position 20 1 19))
    (position-token
     (token-struct '|)| ")" #f #f #f #f #f)
     (position 20 1 19)
     (position 21 1 20))
    (position-token
     (token-struct ':= ":=" #f #f #f #f #f)
     (position 22 1 21)
     (position 24 1 23))
    (position-token
     (token-struct '|{| "{" #f #f #f #f #f)
     (position 25 1 24)
     (position 26 1 25))
    (position-token
     (token-struct 'JSON-STRING "foo" #f #f #f #f #f)
     (position 27 1 26)
     (position 31 1 30))
    (position-token
     (token-struct ': ":" #f #f #f #f #f)
     (position 31 1 30)
     (position 32 1 31))
    (position-token
     (token-struct 'true "true" #f #f #f #f #f)
     (position 33 1 32)
     (position 37 1 36))
    (position-token
     (token-struct '|}| "}" #f #f #f #f #f)
     (position 38 1 37)
     (position 39 1 38)))))

(module+ test
  (check-tokenize
   "call f with [ $x ]"
   (list
    (position-token
     (token-struct 'call "call" #f #f #f #f #f)
     (position 1 1 0)
     (position 5 1 4))
    (position-token
     (token-struct 'FUNCTION-NAME "f" #f #f #f #f #f)
     (position 6 1 5)
     (position 7 1 6))
    (position-token
     (token-struct 'with "with" #f #f #f #f #f)
     (position 8 1 7)
     (position 12 1 11))
    (position-token
     (token-struct '|[| "[" #f #f #f #f #f)
     (position 13 1 12)
     (position 14 1 13))
    (position-token
     (token-struct 'IDENTIFIER "x" #f #f #f #f #f)
     (position 15 1 14)
     (position 17 1 16))
    (position-token
     (token-struct '|]| "]" #f #f #f #f #f)
     (position 18 1 17)
     (position 19 1 18)))))

(module+ test
  (check-tokenize
   "f($a, 42)"
   (list
    (position-token
     (token-struct 'FUNCTION-NAME "f" #f #f #f #f #f)
     (position 1 1 0)
     (position 2 1 1))
    (position-token
     (token-struct '|(| "(" #f #f #f #f #f)
     (position 2 1 1)
     (position 3 1 2))
    (position-token
     (token-struct 'IDENTIFIER "a" #f #f #f #f #f)
     (position 3 1 2)
     (position 5 1 4))
    (position-token
     (token-struct '|,| #\, #f #f #f #f #f)
     (position 5 1 4)
     (position 6 1 5))
    (position-token
     (token-struct 'NUMBER 42 #f #f #f #f #f)
     (position 7 1 6)
     (position 9 1 8))
    (position-token
     (token-struct '|)| ")" #f #f #f #f #f)
     (position 9 1 8)
     (position 10 1 9)))))

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
    (displayln (format "No such file: ~a" file-to-process)
               (current-error-port))
    (exit 1))

  (pretty-print
   (tokenize file-to-process/path)))
