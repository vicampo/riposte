#lang racket/base

(provide program?
         file->program)

(require racket/contract
         racket/match
         (only-in ejs
                  ejsexpr?)
         (only-in racket/hash
                  hash-union)
         (only-in racket/list
                  drop-right)
         (only-in (file "step.rkt")
                  step?)
         (only-in (file "grammar.rkt")
                  parse)
         (only-in (file "tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "expression.rkt")
                  expression?)
         (only-in (file "assignment.rkt")
                  make-normal-assignment
                  make-header-assignment
                  make-parameter-assignment
                  assignment?)
         (only-in (file "assertion.rkt")
                  assertion?
                  make-type-assertion
                  make-adjective-assertion
                  make-negative-adjective-assertion
                  make-equation
                  make-disequation
                  make-response-code-matches-expression
                  make-response-satisfies-schema-assertion
                  response-code-matches-expression?
                  response-satisfies-schema-expression?
                  make-inequality)
         (only-in (file "identifier.rkt")
                  identifier-expression?
                  make-variable-identifier-expression
                  make-environment-variable-identifier-expression
                  make-header-identifier-expression
                  make-parameter-identifier-expression)
         (only-in (file "json.rkt")
                  make-json-object-expression
                  make-json-array-expression)
         (only-in (file "command.rkt")
                  make-command-expression
                  command-expression?)
         (only-in (file "json-pointer.rkt")
                  make-json-pointer-expression)
         (only-in (file "predication.rkt")
                  predication?
                  make-header-absence-predication
                  make-header-presence-predication
                  make-json-pointer-exists-predication)
         (only-in (file "literal.rkt")
                  make-literal)
         (only-in (file "arithmetic.rkt")
                  make-sum)
         (only-in (file "parameters.rkt")
                  param-cwd)
         (only-in (file "./echo.rkt")
                  make-echo-step
                  echo-step?)
         (only-in (file "./exec.rkt")
                  make-exec-step
                  exec-step?)
         (only-in (file "./empty.rkt")
                  make-response-emptiness-expression
                  make-response-nonemptiness-expression
                  make-json-pointer-empty-expression
                  make-json-pointer-nonempty-expression))

(define (program? x)
  (and (list? x)
       (andmap step? x)))

(define/contract (file->program path)
  (path-string? . -> . program?)
  (define path/string
    (cond [(string? path)
           path]
          [(path? path)
           (path->string path)]))
  (define port (open-input-file path))
  (define parse-tree (parse path (make-tokenizer port)))
  (parse-tree->program (syntax->datum parse-tree)))

(define/contract (make-json-object-expr items)
  ((listof any/c) . -> . (listof (cons/c string?
                                         (or/c ejsexpr?
                                               expression?))))
  (match items
    [(list)
     (list)]
    [(list-rest (list 'json-object-item
                      (list 'json-object-property (list 'json-string (? string? prop)))
                      ":"
                      expr)
                more)
     (append (list (cons prop (parse-tree->expression expr)))
             (make-json-object-expr more))]))

(define/contract (parse-tree->expression expr)
  (list? . -> . (or/c expression?
                      ejsexpr?))
  (match expr
    [(list 'expression e)
     (parse-tree->expression e)]
    [(list 'json-string (? string? s))
     s]
    [(list 'json-expression e)
     (parse-tree->expression e)]
    [(list 'json-number (cons 'json-integer digits))
     (string->number (apply string-append
                            digits))]
    [(list 'json-boolean (? string? b))
     (match b
       ["true"
        #t]
       ["false"
        #f])]
    [(list 'json-object "{" items ...)
     (make-json-object-expression
      (make-json-object-expr (remove* (list ",")
                                      (drop-right items 1))))]
    [(list 'json-array "[" items ...)
     (make-json-array-expression
      (map parse-tree->expression
           (remove* (list ",")
                    (drop-right items 1))))]
    [(list 'json-array-item i)
     (parse-tree->expression i)]
    [(list 'id ident)
     (parse-tree->identifier ident)]
    [(list 'json-pointer jp)
     (make-json-pointer-expression jp #f)]
    [(list 'json-pointer jp "relative" "to" identifier)
     (make-json-pointer-expression jp (parse-tree->identifier identifier))]
    [(list 'head-id (? string? name))
     (make-header-identifier-expression name)]
    [(list 'expression expr1 "+" expr2)
     (make-sum (parse-tree->expression expr1)
               (parse-tree->expression expr2))]))

(define/contract (parse-tree->identifier ident-expr)
  (list? . -> . identifier-expression?)
  (match ident-expr
    [(list 'id more)
     (parse-tree->identifier more)]
    [(list 'normal-identifier (? string? ident))
     (make-variable-identifier-expression ident)]
    [(list 'env-identifier (? string? name) "with" "fallback" fallback)
     (make-environment-variable-identifier-expression name
                                                      #:fallback fallback)]
    [(list 'env-identifier (? string? name))
     (make-environment-variable-identifier-expression name)]
    [(list 'head-id (? string? name))
     (make-header-identifier-expression name)]
    [(list 'parameter-identifier (? string? name))
     (make-parameter-identifier-expression name)]))

(define/contract (parse-tree->assignment ass-expr)
  (list? . -> . (or/c assignment?
                      (listof (or/c assignment?
                                    assertion?))))
  (match ass-expr
    [(list 'normal-assignment
           ident
           ":="
           expr)
     (make-normal-assignment (parse-tree->identifier ident)
                             (parse-tree->expression expr))]
    [(list 'normal-assignment
           ident
           ":="
           expr
           "("
           adjective
           ")")
     (define id (parse-tree->identifier ident))
     (define exp (parse-tree->expression expr))
     (define ass (make-normal-assignment id exp))
     (match adjective
       [(list 'json-type (list 'json-number-type (list 'arithmetical-adjective (? string? adj)) (? string? type)))
        (list ass
              (make-type-assertion id (string->symbol type))
              (make-adjective-assertion id (string->symbol adj)))])]
    [(list 'header-assignment
           id
           ":="
           expr)
     (make-header-assignment (parse-tree->identifier id)
                             (parse-tree->expression expr))]
    [(list 'parameter-assignment
           (? string? id)
           ":="
           (? string? expr))
     (make-parameter-assignment id
                                expr)]))

(define/contract (parse-tree->response-code-matches response-code-expr)
  ((or/c string? list?) . -> . response-code-matches-expression?)
  (match response-code-expr
    [(? string?)
     (make-response-code-matches-expression response-code-expr)]
    [(cons 'http-response-code digits)
     (make-response-code-matches-expression
      (apply string-append digits))]))

(define/contract (parse-tree->satisfies-schema-expr schema-expr)
  (list? . -> . response-satisfies-schema-expression?)
  (match schema-expr
    [(list 'schema-ref "in" (? string? filename))
     (make-response-satisfies-schema-assertion filename)]
    [(list 'schema-ref (list 'id identifier))
     (make-response-satisfies-schema-assertion (parse-tree->identifier identifier))]))

(define/contract (parse-tree->command cmd-expr)
  (list? . -> . (or/c command-expression?
                      (cons/c command-expression?
                              (listof assertion?))))
  (match cmd-expr
    [(list-rest 'command (? string? method) more)
     (match more
       [(list (? string? uri))
        (make-command-expression method uri)]
       [(list (? string? uri) (list 'empty "is" "empty"))
        (list (make-command-expression method uri)
              (make-response-emptiness-expression))]
       [(list (? string? uri) (list 'empty "is" "non" "empty"))
        (list (make-command-expression method uri)
              (make-response-nonemptiness-expression))]
       [(list id
              (? string? uri))
        (make-command-expression method
                                 uri
                                 #:payload (parse-tree->identifier id))]
       [(list id
              (? string? uri)
              (list 'empty "is" "empty"))
        (list
         (make-command-expression method
                                  uri
                                  #:payload (parse-tree->identifier id))
         (make-response-emptiness-expression))]
       [(list id
              (? string? uri)
              (list 'empty "is" "non" "empty"))
        (list
         (make-command-expression method
                                  uri
                                  #:payload (parse-tree->identifier id))
         (make-response-nonemptiness-expression))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code))
        (list
         (make-command-expression method
                                 uri
                                 #:payload (parse-tree->identifier id))
         (parse-tree->response-code-matches code))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'empty "is" "empty"))
        (list (make-command-expression method uri)
              (parse-tree->response-code-matches code)
              (make-response-emptiness-expression))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'empty "is" "non" "empty"))
        (list (make-command-expression method uri)
              (parse-tree->response-code-matches code)
              (make-response-nonemptiness-expression))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'empty "is" "empty"))
        (list
         (make-command-expression method
                                  uri
                                  #:payload (parse-tree->identifier id))
         (parse-tree->response-code-matches code)
         (make-response-emptiness-expression))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'empty "is" "non" "empty"))
        (list
         (make-command-expression method
                                  uri
                                  #:payload (parse-tree->identifier id))
         (parse-tree->response-code-matches code)
         (make-response-nonemptiness-expression))]
       [(list (? string? uri)
              (list 'satisfies "satisfies" "schema" schema))
        (list (make-command-expression method uri)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "empty"))
        (list (make-command-expression method uri)
              (make-response-emptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "non" "empty"))
        (list (make-command-expression method uri)
              (make-response-nonemptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema))
        (list (make-command-expression method uri)
              (parse-tree->response-code-matches code)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "empty"))
        (list (make-command-expression method uri)
              (parse-tree->response-code-matches code)
              (make-response-emptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "non" "empty"))
        (list (make-command-expression method uri)
              (parse-tree->response-code-matches code)
              (make-response-nonemptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema))
        (list (make-command-expression method
                                       uri
                                       #:payload (parse-tree->identifier id))
              (parse-tree->response-code-matches code)
              (parse-tree->satisfies-schema-expr schema))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "empty"))
        (list (make-command-expression method
                                       uri
                                       #:payload (parse-tree->identifier id))
              (parse-tree->response-code-matches code)
              (make-response-emptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list id
              (? string? uri)
              (list 'responds-with "responds" "with" code)
              "and"
              (list 'satisfies "satisfies" "schema" schema)
              "and"
              (list 'empty "is" "non" "empty"))
        (list (make-command-expression method
                                       uri
                                       #:payload (parse-tree->identifier id))
              (parse-tree->response-code-matches code)
              (make-response-nonemptiness-expression)
              (parse-tree->satisfies-schema-expr schema))]
       [(list (? string? uri)
              (list 'responds-with "responds" "with" code))
        (list
         (make-command-expression method uri)
         (parse-tree->response-code-matches code))])]))

(define/contract (parse-tree->assertion assert-expr)
  (list? . -> . (or/c assertion?
                      (listof assertion?)))
  (match assert-expr
    [(list 'equality lhs-expr "=" rhs-expr)
     (make-equation (parse-tree->expression lhs-expr)
                    (parse-tree->expression rhs-expr))]
    [(list 'disequality lhs-expr "!=" rhs-expr)
     (make-disequation (parse-tree->expression lhs-expr)
                       (parse-tree->expression rhs-expr))]
    [(list 'inequality lhs-expr op rhs-expr)
     (define lhs (parse-tree->expression lhs-expr))
     (define rhs (parse-tree->expression rhs-expr))
     (match op
       ["<"
        (make-inequality lhs rhs #:strict? #t)]
       ["<="
        (make-inequality lhs rhs #:strict? #f)]
       [">"
        (make-inequality rhs lhs #:strict? #t)]
       [">="
        (make-inequality rhs lhs #:strict? #f)])]
    [(list-rest 'predication more)
     (match more
       [(list (? string? header) "is" "absent")
        (make-header-absence-predication header)]
       [(list (? string? header) "is" "present")
        (make-header-presence-predication header)]
       [(list expr "is" (list 'json-type type))
        (match type
          [(list 'json-sequence-type (? string? t))
           (make-type-assertion (parse-tree->expression expr)
                                (string->symbol t))]
          [(list 'json-sequence-type (? list? adjectives) (? string? t))
           (define type-ass
             (make-type-assertion (parse-tree->expression expr)
                                  (string->symbol t)))
           (define adjective
             (match adjectives
               [(list 'sequence-adjective (? string? adj))
                (make-adjective-assertion (parse-tree->expression expr)
                                          (string->symbol adj))]
               [(list 'sequence-adjective "non" (? string? adj))
                (make-negative-adjective-assertion (parse-tree->expression expr)
                                                   (string->symbol adj))]))
           (list
            type-ass
            adjective)])]
       [(list (list 'json-pointer (? string? jp)) "exists")
        (make-json-pointer-exists-predication jp #f #t)]
       [(list (list 'json-pointer (? string? jp)) "does" "not" "exist")
        (make-json-pointer-exists-predication jp #f #f)]
       [(list (list 'json-pointer (? string? jp)) "exists" "and" "is" "empty")
        (list (make-json-pointer-exists-predication jp #f #t)
              (make-json-pointer-empty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "exists" "and" "is" "non" "empty")
        (list (make-json-pointer-exists-predication jp #f #t)
              (make-json-pointer-nonempty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "does" "not" "exist" "and" "is" "empty")
        (list (make-json-pointer-exists-predication jp #f #t)
              (make-json-pointer-empty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "does" "not" "exist" "and" "is" "non" "empty")
        (list (make-json-pointer-exists-predication jp #f #t)
              (make-json-pointer-nonempty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "exists" "relative" "to" id)
        (make-json-pointer-exists-predication jp (parse-tree->identifier id) #t)]
       [(list (list 'json-pointer (? string? jp)) "exists" "relative" "to" id "and" "is" "empty")
        (list (make-json-pointer-exists-predication jp (parse-tree->identifier id) #t)
              (make-json-pointer-empty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "exists" "relative" "to" id "and" "is" "non" "empty")
        (list (make-json-pointer-exists-predication jp (parse-tree->identifier id) #t)
              (make-json-pointer-nonempty-expression jp #f))]
       [(list (list 'json-pointer (? string? jp)) "does" "not" "exist" "relative" "to" id)
        (make-json-pointer-exists-predication jp (parse-tree->identifier id) #f)]
       [else
        (error (format "cannot handle predication ~a" assert-expr))])]))

(define/contract (load-riposte-file path)
  (path? . -> . (or/c false/c list?))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-error "error occurred during expanding an import: ~a" (exn-message e))
                               #f)])
    (define port (open-input-file path))
    (define parse-tree (parse path (make-tokenizer port)))
    (syntax->datum parse-tree)))

(define/contract (parse-tree->echo to-be-echoed)
  ((or/c false/c list?) . -> . echo-step?)
  (match to-be-echoed
    [#f
     (make-echo-step #f)]
    [(list 'normal-identifier id)
     (make-echo-step (parse-tree->identifier to-be-echoed))]
    [(list 'head-id id)
     (make-echo-step (parse-tree->identifier to-be-echoed))]
    [else
     (error (format "parse-tree->echo: Cannot make sense of ~a" to-be-echoed))]))

(define/contract (parse-tree->exec to-exec)
  (string? . -> . exec-step?)
  (define cwd (param-cwd))
  (unless (path? cwd)
    (error "Current working directory not set!"))
  (define p (build-path cwd to-exec))
  (unless (file-exists? p)
    (error (format "No such file: ~a" (path->string p))))
  (define permissions (file-or-directory-permissions p))
  (unless (list? (member 'execute permissions))
    (error (format "File is not executable: ~a" (path->string p))))
  (make-exec-step p))

(define/contract (parse-tree->step step-expr)
  (list? . -> . (or/c step?
                      (listof step?)
                      program?))
  (match step-expr
    [(list 'program-step step)
     (parse-tree->step step)]
    [(list 'assignment (? list? ass))
     (parse-tree->assignment ass)]
    [(list 'echo "echo")
     (parse-tree->echo #f)]
    [(list 'echo "echo" more)
     (parse-tree->echo more)]
    [(cons 'command more)
     (parse-tree->command step-expr)]
    [(list 'import "import" (? string? to-import))
     (define cwd (param-cwd))
     (unless (path? cwd)
       (error "Current working directory not set!"))
     (define p (build-path cwd to-import))
      (unless (file-exists? p)
        (error (format "No such file: ~a" (path->string p))))
      (define imported-program (load-riposte-file p))
      (unless (list? imported-program)
        (error (format "Malformed Riposte file: ~a" (path->string p))))
      (parse-tree->program imported-program)]
    [(list 'exec "exec" (? string? to-exec))
      (parse-tree->exec to-exec)]
    [(list 'assertion ass)
     (parse-tree->assertion ass)]))

(define/contract (flatten-program steps)
  (list? . -> . program?)
  (match steps
    [(list)
     (list)]
    [(cons (? step? s) more)
     (cons s (flatten-program more))]
    [(cons (? list? ss) more)
     (append (flatten-program ss)
             (flatten-program more))]))

(define/contract (parse-tree->program program)
  (list? . -> . program?)
  (match program
    [(cons 'riposte-program (? list? steps))
     (flatten-program (map parse-tree->step steps))]
    [(list 'program-step (? list? more))
     (flatten-program (parse-tree->step more))]))
