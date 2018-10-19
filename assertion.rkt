#lang racket/base

(provide assertion%
         assertion?
         make-response-code-matches-expression
         make-type-assertion
         make-adjective-assertion
         make-negative-adjective-assertion
         make-equation
         make-response-satisfies-schema-assertion
         response-satisfies-schema-expression?
         response-code-matches-expression?
         make-inequality
         make-disequation)

(require racket/class
         racket/contract
         racket/match
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string
                  equal-ejsexprs?
                  port->ejsexpr)
         (only-in racket/list
                  empty?
                  first
                  rest)
         (only-in argo
                  json-schema?
                  adheres-to-schema?)
         (only-in (file "step.rkt")
                  step%)
         (only-in (file "environment.rkt")
                  environment?
                  environment-response
                  environment-response-code)
         (only-in (file "expression.rkt")
                  expression?)
         (only-in (file "identifier.rkt")
                  identifier-expression?)
         (only-in (file "parameters.rkt")
                  param-cwd)
         (only-in (file "util.rkt")
                  render-ejsexprish
                  ensure-ejsexpr))

(define assertion%
  (class step%
    (super-new)))

(define/contract (assertion? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x assertion%)))

(define/contract (code-char-matches-pattern-char? code-char pattern-char)
  (char? char? . -> . boolean?)
  (or (char=? #\X pattern-char)
      (char=? code-char pattern-char)))

(define/contract (code-string-matches-pattern? code pattern)
  ((listof char?) (listof char?) . -> . boolean?)
  (cond [(empty? code)
         (empty? pattern)]
        [(empty? pattern)
         #f]
        [(code-char-matches-pattern-char? (first code) (first pattern))
         (code-string-matches-pattern? (rest code)
                                       (rest pattern))]
        [else
         #f]))

(define/contract (code-matches? code pattern)
  (exact-integer? (or/c exact-integer? string?) . -> . boolean?)
  (cond [(exact-integer? pattern)
         (= code pattern)]
        [else
         (code-string-matches-pattern? (string->list (format "~a" code))
                                       (string->list pattern))]))

(define response-code-matches-expression%
  (class assertion%
    (super-new)
    (init-field code)
    (define/override (evaluate env)
      (define c (environment-response-code env))
      (match c
        [#f
         (error "No response has been received; cannot evaluate!")]
        [(? exact-nonnegative-integer?)
         (unless (code-matches? c code)
           (error (format "Received code ~a does not match expected code ~a."
                          c
                          code)))
         env]
        [else
         (error (format "WTF kind of response is this? ~a" c))]))
    (define/override (render)
      (format "+code matches ~a" code))))

(define/contract (response-code-matches-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x response-code-matches-expression%)))

(define/contract (make-response-code-matches-expression response-code)
  (string? . -> . response-code-matches-expression?)
  (new response-code-matches-expression%
       [code response-code]))

(define adjective-assertion%
  (class assertion%
    (init-field term
                adjective
                [negate? #f])
    (super-new)
    (define/override (evaluate env)
      (define v (send term evaluate env))
      ;; all our adjectives so fare are numeric. So first check
      ;; that the term works out to a number:
      (define ok?
        (match adjective
          ['positive
           (and (real? v)
                (positive? v))]
          ['negative
           (and (real? v)
                (negative? v))]
          ['empty
           (or (and (list? v)
                    (empty? v))
               (and (string? v)
                    (string=? "" v))
               (and (hash? v)
                    (hash-empty? v)))]))
      (cond [negate?
             (when ok?
               (error (format "~a is not ~a (its value is ~a)!"
                              (send term render)
                              adjective
                              v)))]
            [else
             (unless ok?
               (error (format "~a is ~a (its value is ~a)!"
                              (send term render)
                              adjective
                              v)))])
      env)
    (define/override (render)
      (error "Cannot render adjective ssertions!"))))

(define (adjective-assertion? x)
  (and (object? x)
       (is-a? x adjective-assertion%)))

(define/contract (make-adjective-assertion expr adj)
  (expression? (one-of/c 'positive
                         'negative
                         'empty)
               . -> . adjective-assertion?)
  (new adjective-assertion%
       [term expr]
       [adjective adj]))

(define/contract (make-negative-adjective-assertion expr adj)
  (expression? (one-of/c 'positive
                         'negative
                         'empty)
               . -> . adjective-assertion?)
  (new adjective-assertion%
       [term expr]
       [adjective adj]
       [negate? #t]))

(define type-assertion%
  (class assertion%
    (super-new)
    (init-field term type)
    (define/override (evaluate env)
      (define v (send term evaluate env))
      (define ok?
        (match type
          ['null
           (eq? v 'null)]
          ['boolean
           (boolean? v)]
          ['string
           (string? v)]
          ['number
           (number? v)]
          ['integer
           (integer? v)]
          ['array
           (list? v)]
          ['object
           (hash? v)]))
      (unless ok?
        (error (format "Type assertion failed: ~a does not have the type ~a."
                       (send term render)
                       type)))
      env)
    (define/override (render)
      (error "Cannot render type assertions!"))))

(define (type-assertion? x)
  (and (object? x)
       (is-a? x type-assertion%)))

(define/contract (make-type-assertion term json-type)
  (expression? (one-of/c 'null
                         'boolean
                         'string
                         'number
                         'integer
                         'array
                         'object)
               . -> . type-assertion?)
  (new type-assertion%
       [term term]
       [type json-type]))

(define equation%
  (class assertion%
    (super-new)
    (init-field lhs rhs)
    (define/override (evaluate env)
      (define l (ensure-ejsexpr lhs env))
      (define r (ensure-ejsexpr rhs env))
      (unless (equal-ejsexprs? l r)
        (error (format "~a (which evaluates to ~a) is not equal to ~a (which evaluates to ~a)!"
                       (render-ejsexprish lhs)
                       (ejsexpr->string l)
                       (render-ejsexprish rhs)
                       (ejsexpr->string r))))
      env)
    (define/override (render)
      (define l (render-ejsexprish lhs))
      (define r (render-ejsexprish rhs))
      (format "~a = ~a" l r))))

(define/contract (equation? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x equation%)))

(define/contract (make-equation left right)
  ((or/c ejsexpr? expression?) (or/c ejsexpr? expression?) . -> . equation?)
  (new equation%
       [lhs left]
       [rhs right]))

(define response-satisfies-schema-expression%
  (class assertion%
    (super-new)
    (init-field schema)
    (define/override (evaluate env)
      (define loaded-schema
        (cond [(expression? schema)
               (send schema evaluate env)]
              [else
               (define cwd (param-cwd))
               (unless (path? cwd)
                 (error "Current working directory not set!"))
               (define path
                 (cond [(absolute-path? schema)
                        (string->path schema)]
                       [else
                        (build-path cwd schema)]))
               (unless (file-exists? path)
                 (error (format "No such file: ~a" (path->string path))))
               (define port (open-input-file path))
               (define s (port->ejsexpr port))
               (close-input-port port)
               s]))
      (unless (json-schema? loaded-schema)
        (error (format "JSON content of ~a is not really a JSON Schema."
                       (cond [(expression? schema)
                              (send schema render)]
                             [else
                              schema]))))
      (define data (environment-response env))
      (define ok? (adheres-to-schema? data loaded-schema))
      (unless ok?
        (error
         (cond [(expression? schema)
                (format "Response does not satisfy schema contained in ~a! Response was: ~a"
                        (send schema render)
                        data)]
               [else
                (format "Response does not satisfy schema in file \"~a\". Response was: ~a"
                        schema
                        data)])))
      env)
    (define/override (render)
      (error "Cannot render response-satisfies-schema assertion!"))))

(define (response-satisfies-schema-expression? x)
  (and (object? x)
       (is-a? x response-satisfies-schema-expression%)))

(define/contract (make-response-satisfies-schema-assertion s)
  ((or/c identifier-expression?
         string?)
   . -> . response-satisfies-schema-expression?)
  (new response-satisfies-schema-expression%
       [schema s]))

(define/contract inequality%
  class?
  (class assertion%
    (super-new)
    (init-field lhs rhs strict?)
    (define/override (evaluate env)
      (define l (ensure-ejsexpr lhs env))
      (define r (ensure-ejsexpr rhs env))
      (unless (real? l)
        (error (format "Left-hand side of ~a is not a number!"
                       (send this render))))
      (unless (real? r)
        (error (format "Right-hand side of ~a is not a number!"
                       (send this render))))
      (define ok?
        (cond [strict?
               (< l r)]
              [else
               (<= l r)]))
      (unless ok?
        (error (format "~a fails! Left-hand side works out to ~a, whereas the right-hand side is ~a."
                       (send this render)
                       l
                       r)))
      env)
    (define/override (render)
      (define l (render-ejsexprish lhs))
      (define r (render-ejsexprish rhs))
      (cond [strict?
             (format "~a < ~a" l r)]
            [else
             (format "~a <= ~a" l r)]))))

(define/contract (inequality? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x inequality%)))

(define/contract (make-inequality left right #:strict? [strict? #f])
  (->* ((or/c expression?
              ejsexpr?)
        (or/c expression?
              ejsexpr?))
       (#:strict? boolean?)
       inequality?)
  (new inequality%
       [lhs left]
       [rhs right]
       [strict? strict?]))

(define disequation%
  (class assertion%
    (super-new)
    (init-field lhs rhs)
    (define/override (evaluate env)
      (define l (ensure-ejsexpr lhs env))
      (define r (ensure-ejsexpr rhs env))
      (when (equal-ejsexprs? l r)
        (error (format "~a (which evaluates to ~a) is equal to ~a (which evaluates to ~a)!"
                       (render-ejsexprish lhs)
                       (ejsexpr->string l)
                       (render-ejsexprish rhs)
                       (ejsexpr->string r))))
      env)
    (define/override (render)
      (define l (render-ejsexprish lhs))
      (define r (render-ejsexprish rhs))
      (format "~a != ~a" l r))))

(define/contract (disequation? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x disequation%)))

(define/contract (make-disequation left right)
  ((or/c ejsexpr? expression?) (or/c ejsexpr? expression?) . -> . disequation?)
  (new disequation%
       [lhs left]
       [rhs right]))
