#lang br/quicklang

(provide make-tokenizer
         tokenize-file)

(require brag/support
         racket/contract)

#|

Identifiers: $ followed by a sequence of letters, numbers, and '_'

|#

(define expecting-uri? #f)

(define (make-tokenizer port)
  (define (next-uri-token)
    (define uri-lexer
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
       [(union #\newline #\tab #\space #\u00A0)
        (token 'WHITESPACE
               lexeme
               #:skip? #t)]
       [(:: "$"
            (:+ (union (:/ "A" "Z" "a" "z" "0" "9")
                       "_")))
        (token 'IDENTIFIER
               (substring lexeme 1))]
       [(from/stop-before
         (:: (:* (:~ (union #\newline #\tab #\space #\{)))
             "{"
             (:* (:~ (union #\newline #\tab #\space #\})))
             "}"
             (:* (:~ (union #\newline #\tab #\space))))
         (union #\newline #\tab #\space))
        (token 'URI-TEMPLATE
               (string-trim lexeme))]
       [(from/stop-before
         (:+ (:~ (union #\newline #\tab #\space)))
         (union #\newline #\tab #\space))
        (token 'URI
               (string-trim lexeme))]))
    (define tok (uri-lexer port))
    (when (and (position-token? tok)
               (token-struct? (position-token-token tok))
               (member (token-struct-type (position-token-token tok))
                       (list 'URI 'URI-TEMPLATE)))
      (set! expecting-uri? #f))
    tok)
  (define (next-riposte-token)
    (define riposte-lexer
      (lexer-src-pos
       [(eof)
        eof]
       [(union #\newline #\space #\tab #\u00A0)
        (token 'WHITESPACE
               lexeme
               #:skip? #t)]
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
       ;; global identifiers
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
       ;; JSON Pointer expressions
       [(from/stop-before
         (::
          (union "/"
                 (:+ (:: "/" (union (:/ "A" "Z" "a" "z" "0" "9")
                                    "-"
                                    "_"
                                    "\\")))))
         (union #\newline #\tab #\space))
        (token 'JSON-POINTER lexeme)]
       ;; keywords
       [(:: "at"
            (union #\newline #\tab #\space))
        (token 'AT "at")]
       [(union ":="
               "!="
               "="
               "<"
               ">"
               "is"
               "and"
               "has"
               "least"
               "most"
               "properties"
               "elements"
               "characters"
               "not"
               "non"
               "does"
               "to"
               "satisfy"
               "satisfies"
               "schema"
               "empty"
               "positive"
               "negative"
               "responds"
               "with"
               "fallback"
               "headers"
               "respond"
               "in"
               "*"
               "present"
               "exists"
               "absent"
               "length"
               "import"
               "unset")
        (token lexeme lexeme)]
       ;; JSON keywords
       [(union "true"
               "false"
               "null"
               "number"
               "integer"
               "float"
               "string"
               "array"
               "object")
        (token lexeme lexeme)]
       ;; HTTP methods
       [(from/stop-before (union "GET"
                                 "PUT"
                                 "POST"
                                 "PATCH"
                                 "DELETE")
         (union #\newline #\tab #\space))
        (token 'HTTP-METHOD lexeme)]
       [(from/to #\" #\")
        (token 'DOUBLE-QUOTED-STRING
               (trim-ends "\"" lexeme "\""))]
       [(:: (union "1" "2" "3" "4" "5")
            (union "1" "2" "3" "4" "5")
            "X")
        (token 'HTTP-RESPONSE-CODE-PATTERN
               lexeme)]
       [(:: (union "1" "2" "3" "4" "5")
            "X"
            "X")
        (token 'HTTP-RESPONSE-CODE-PATTERN
               lexeme)]
       [(:/ "0" "9")
        (token 'DIGIT lexeme)]
       [(:/ "A" "Z" "a" "z")
        (token 'LETTER (string-ref lexeme 0))]
       [any-char
        (token lexeme lexeme)
        ;; (token (format "FALLBACK-~a" (char->integer (string-ref lexeme 0)))
        ;;        lexeme)
        ]))
    (riposte-lexer port))
  ;; here we need to have the two tokenizers cooperate with each
  (define (next-token)
    (cond [expecting-uri?
           (next-uri-token)]
          [else
           (define tok (next-riposte-token))
           (when (and (position-token? tok)
                      (token-struct? (position-token-token tok))
                      (or (eq? 'HTTP-METHOD
                               (token-struct-type
                                (position-token-token tok)))
                          (list? (member (token-struct-val
                                          (position-token-token tok))
                                         (list "in" "at")))))
             (set! expecting-uri? #t))
           tok]))
  (define counting? (port-counts-lines? port))
  next-token)

(define/contract (tokenize-file path)
  (path-string? . -> . list?)
  (define (lex-it)
    (apply-tokenizer-maker make-tokenizer (current-input-port)))
  (with-input-from-file path lex-it #:mode 'text))

(module+ main
  (tokenize-file "/Users/jessealama/riposte/examples/big.rip"))
