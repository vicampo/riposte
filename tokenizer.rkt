#lang br/quicklang

(provide make-tokenizer
         tokenize-file)

(require brag/support
         racket/contract)

(module+ test
  (require rackunit))

#|

Identifiers: $ followed by a sequence of letters, numbers, and '_'

|#

(define expecting-uri? #f)
(define parsed-base-url-parameter? #f)

(define-lex-trans keyword/no-whitespace
  (λ (stx)
    (syntax-case stx ()
      [(_ KW)
       #'(:seq
          (:* (union #\newline #\space #\tab #\u00A0))
          KW
          (:+ (union #\newline #\space #\tab #\u00A0)))])))

(define-lex-trans keyword
  (λ (stx)
    (syntax-case stx ()
      [(_ KW)
       #'(:seq
          (:+ (union #\newline #\space #\tab #\u00A0))
          KW
          (:+ (union #\newline #\space #\tab #\u00A0)))])))

(define (make-tokenizer port)
  (define (next-riposte-token)
    (define riposte-lexer
      (lexer-src-pos
       [(eof)
        eof]
       [(from/to "#" "\n")
        (token 'COMMENT
                        lexeme
                        #:skip? #t)]
       [(:: "#"
            (:* (:~ #\newline)))
        (token 'EOF-COMMENT
               lexeme
               #:skip? #t)]
       ;; normal identifiers
       [(:: "$"
            (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                       "_")))
        (token 'IDENTIFIER
               (substring lexeme 1))]
       ;; parameter identifiers
       [(:: "%"
            (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                       "_")))
        (token 'PARAMETER-IDENTIFIER
               (substring lexeme 1))]
       [(:: "@"
            (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                       "_")))
        (token 'ENV-IDENTIFIER
               (substring lexeme 1))]
       ;; header identifiers
       [(:: "^"
             (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                        "_"
                        "-")))
        (token 'HEADER-IDENTIFIER
               (string-trim
                (substring lexeme 1)))]
       [(from/stop-before
         (:: (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                        "_"
                        "-"))
             "^")
         (union #\newline #\tab #\space))
        (token 'HEADER-REF
               (let ([trimmed (string-trim lexeme)])
                 (substring trimmed
                            0
                            (sub1 (string-length trimmed)))))]
       ;; keywords
       [(keyword "at")
        (token 'at "at")]
       [(keyword "exec")
        (token "exec" "exec")]
       [(keyword ":=")
        (token ":=" ":=")]
       [(keyword "!=")
        (token "!=" "!=")]
       [(keyword "=")
        (token "=" "=")]
       [(keyword "<")
        (token "<" "<")]
       [(keyword ">")
        (token ">" ">")]
       [(keyword "is")
        (token "is" "is")]
       [(keyword "and")
        (token "and" "and")]
       [(keyword "has")
        (token "has" "has")]
       [(keyword "least")
        (token "least" "least")]
       [(keyword "most")
        (token "most" "most")]
       [(keyword "properties")
        (token "properties" "properties")]
       [(keyword "elements")
        (token "elements" "elements")]
       [(keyword "characters")
        (token "characters" "characters")]
       [(keyword "not")
        (token "not" "not")]
       [(keyword "non")
        (token "non" "non")]
       [(keyword "does")
        (token "does" "does")]
       [(keyword "relative to")
        (token "relative to" "relative to")]
       #;
       [(keyword "to")
        (token "to" "to")]
       [(keyword "satisfy")
        (token "satisfy" "satisfy")]
       [(keyword "satisfies")
        (token "satisfies" "satisfies")]
       [(keyword "schema")
        (token "schema" "schema")]
       [(keyword "empty")
        (token "empty" "empty")]
       [(keyword "positive")
        (token "positive" "positive")]
       [(keyword "negative")
        (token "negative" "negative")]
       [(keyword "responds with")
        (token "responds with" "responds with")]
       #;
       [(keyword "responds")
        (token 'responds "responds")]
       [(keyword "with")
        (token "with" "with")]
       [(keyword "with fallback")
        (token "with fallback" "with fallback")]
       #;
       [(keyword "fallback")
        (token "fallback" "fallback")]
       [(keyword "headers")
        (token "headers" "headers")]
       [(keyword "respond")
        (token "respond" "respond")]
       [(keyword "in")
        (token "in" "in")]
       [(keyword "present")
        (token "present" "present")]
       [(keyword "exist")
        (token "exist" "exist")]
       [(keyword "exists")
        (token "exists" "exists")]
       [(keyword "absent")
        (token "absent" "absent")]
       [(keyword "length")
        (token "length" "length")]
       [(keyword "unset")
        (token "unset" "unset")]
       [(keyword "echo")
        (token "echo" "echo")]
       [(keyword/no-whitespace "import")
        (token 'import "import")]
       [(keyword "true")
        (token "true" "true")]
       [(keyword "false")
        (token "false" "false")]
       [(keyword "null")
        (token "null" "null")]
       [(keyword "number")
        (token "number" "number")]
       [(keyword "integer")
        (token "integer" "integer")]
       [(keyword "float")
        (token "float" "float")]
       [(keyword "string")
        (token "string" "string")]
       [(keyword "array")
        (token "array" "array")]
       [(keyword "object")
        (token "object" "object")]
       [(from/stop-before (union "GET"
                                 "POST"
                                 "PATCH"
                                 "OPTIONS"
                                 "PUT"
                                 "DELETE"
                                 "HEAD")
                          (union #\newline #\tab #\space))
        (token 'HTTP-METHOD lexeme)]
       [(from/to #\" #\")
        (token 'DOUBLE-QUOTED-STRING
               (trim-ends "\"" lexeme "\""))]
       [(:: (union "1" "2" "3" "4" "5" "x" "X")
            (union "1" "2" "3" "4" "5" "x" "X")
            (union "X" "x"))
        (token 'HTTP-RESPONSE-CODE-PATTERN
               lexeme)]
       [(:: (union "1" "2" "3" "4" "5" "x" "X")
            (union "X" "x")
            (union "X" "x"))
        (token 'HTTP-RESPONSE-CODE-PATTERN
               lexeme)]
       [(:/ "2" "9")
        (token 'NON-ZERO-NON-ONE-DIGIT lexeme)]
       [#\0
        (token 'ZERO lexeme)]
       [#\1
        (token 'ONE lexeme)]
       [(:/ #\A #\Z)
        (token 'UPPERCASE-LETTER lexeme)]
       [(:/ #\a #\z)
        (token 'LOWERCASE-LETTER lexeme)]
       [#\:
        (token 'COLON lexeme)]
       [#\/
        (token 'SLASH lexeme)]
       [#\.
        (token 'PERIOD lexeme)]
       [#\-
        (token 'DASH lexeme)]
       [#\_
        (token 'UNDERSCORE lexeme)]
       [#\{
        (token 'OPEN-BRACE lexeme)]
       [#\}
        (token 'CLOSE-BRACE lexeme)]
       [#\[
        (token 'OPEN-BRACKET lexeme)]
       [#\]
        (token 'CLOSE-BRACKET lexeme)]
       [#\,
        (token 'COMMA lexeme)]
       [#\?
        (token 'QUESTION-MARK lexeme)]
       [#\*
        (token 'ASTERISK lexeme)]
       [#\+
        (token 'PLUS lexeme)]
       [(union #\newline #\space #\tab #\u00A0)
        (token 'WHITESPACE
               (string-ref lexeme 0)
               #:skip? #f)]
       [any-char
        (token 'CHAR (string-ref lexeme 0))]))
    (riposte-lexer port))
  (define counting? (port-counts-lines? port))
  next-riposte-token)

(define/contract (tokenize-string str)
  (string? . -> . list?)
  (define (lex-it)
    (apply-tokenizer-maker make-tokenizer (current-input-port)))
  (with-input-from-string str lex-it))

(module+ test
  (test-case "Support big X and little x"
    (check-not-exn
     (lambda ()
       (tokenize-string "GET foo responds with 2xx")))
    (check-not-exn
     (lambda ()
       (tokenize-string "GET foo responds with 20x")))
    (check-not-exn
     (lambda ()
       (tokenize-string "GET foo responds with 2Xx")))
    (check-not-exn
     (lambda ()
       (tokenize-string "GET foo responds with 2xX")))))

(define/contract (tokenize-file path)
  (path-string? . -> . list?)
  (define (lex-it)
    (apply-tokenizer-maker make-tokenizer (current-input-port)))
  (with-input-from-file path lex-it #:mode 'text))

(module+ main

  (require racket/cmdline)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (tokenize-file file-to-process))
