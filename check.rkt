#lang racket

(provide check-program)

(require (file "environment.rkt")
         (file "util.rkt")
         racket/contract
         racket/list)

(module+ test
  (require rackunit))

(define/contract (check-command cmd env)
  (string? environment? . -> . environment?)
  env)

(define/contract (check-identifier identifier env)
  ((or/c string? list?) environment? . -> . environment?)
  (match identifier
    [(? string?)
     (with-handlers ([exn:fail:contract? (lambda (e)
                                           (error (format "Variable used before defined (~a)." identifier)))])
       (lookup-variable identifier env))
     env]
    [(list 'global-identifier id)
     (unless (known-global-variable? id)
       (error (format "Unknown global variable (~a)." id)))
     env]
    [(list 'normal-identifier id)
     (check-identifier id env)]
    [(list 'env-identifier (? string? id))
     (when (eq? #f (getenv id))
          (error (format "Environment variable ~a unset." id)))
     env]
    [(list 'env-identifier (? string? id) "with" "fallback" expr)
     (check-expression expr env)]
    [else
     (error (format "Cannot check identifier ~a" identifier))]))

(define/contract (check-json-object-property prop env)
  (list? environment? . -> . environment?)
  (match prop
    [(list 'json-object-property (list 'json-string s))
     env]
    [else
     (error (format "Cannot check JSON object property ~a" prop))]))

(define (check-json-array-item item env)
  (match item
    [","
     env]
    [(list 'json-array-item js)
     (check-json-expression js env)]
    [else
     (error (format "Don't know how to check JSON array item ~a" item))]))

(define (check-json-object-item item env)
  (match item
    [","
     env]
    [(list 'json-object-item property ":" val)
     (check-json-object-property property env)
     (check-expression val env)]
    [else
     (error (format "Don't know how to check JSON object item ~a" item))]))

(define (json-object-properties items)
  (cond ((empty? items)
         (list))
        (else
         (define x (first items))
         (match x
           [","
            (json-object-properties (rest items))]
           [(list 'json-object-item (list 'json-object-property (list 'json-string s)) ":" whatever)
            (cons s (json-object-properties (rest items)))]
           [else
            (error (format "Cannot make sense of JSON object property ~a" x))]))))

(define (check-json-object-items items env)
  (map (lambda (x) (check-json-object-item x env))
       items)
  (define dupe (check-duplicates (json-object-properties items)))
  (unless (eq? dupe #f)
    (error (format "Object property \"~a\" repeated." dupe)))
  env)

(define (check-json-array-items items env)
  (map (lambda (x) (check-json-array-item x env))
       items)
  env)

(define/contract (check-json-expression json env)
  (list? environment? . -> . environment?)
  (match json
    [(list 'json-expression js)
     (check-json-expression js env)]
    [(list 'json-string s)
     env]
    [(list 'json-number n)
     env]
    [(list 'json-object "{" items ... "}")
     (check-json-object-items items env)]
    [(list 'json-array "[" items ... "]")
     (check-json-array-items items env)]
    [else
     (error (format "Cannot check JSON expression ~a" json))]))

(define/contract (check-expression expr env)
  ((or/c list? string?) environment? . -> . environment?)
  (match expr
    [(? string?)
     env]
    [(list 'expression more)
     (check-expression more env)]
    [(list 'json-expression json)
     (check-json-expression json env)]
    [(list 'id id)
     (check-identifier id env)]
    [(list 'json-pointer jp)
     env]
    [(list 'head-id id)
     env]
    [(list 'expression expr1 (or "+" "*") expr2)
     (check-expression expr1 env)
     (check-expression expr2 env)
     env]
    [else
     (error (format "Cannot check expression ~a" expr))]))

(define/contract (render-identifier identifier)
  (list? . -> . string?)
  (match identifier
    [(list 'global-identifier id)
     id]
    [(list 'normal-identifier id)
     id]
    [else
     (error (format "Cannot render identifier: ~a" identifier))]))

(define/contract (check-assignment ass env)
  (list? environment? . -> . environment?)
  (match ass
    [(list 'normal-assignment (list 'id id) ":=" expr)
     (check-expression expr env)
     (extend-environment env
                         (string->symbol (render-identifier id))
                         #f)]
    [(list 'normal-assignment (list 'id id) ":=" expr "(" type ")")
     (check-expression expr env)
     (extend-environment env
                         (string->symbol (render-identifier id))
                         #f)]
    [(list 'header-assignment header ":=" expr)
     (check-expression expr env)
     (extend-environment/header env
                                (string->symbol header)
                                "")]
    [else
     (error (format "Cannot check assignment ~a" ass))]))

(define/contract (check-uri uri env)
  (string? environment? . -> . environment?)
  (unless (string? uri)
    (error (format "Cannot check URI ~a" uri)))
  env)

(define (check-headers hs env)
  (match hs
    [(list 'normal-identifier id)
     (check-identifier hs env)]
    [else
     (error (format "Cannot check headers ~a" hs))]))

(define (check-code code env)
  (match code
    [(list 'http-response-code c)
     (unless (string? c)
       (error (format "Cannot check code ~a" code)))
     env]
    [else
     (error (format "Cannot check code ~a" code))]))

(define/contract (check-assertion ass env)
  (list? environment? . -> . environment?)
  (match ass
    [(list 'equality lhs "=" rhs)
     (check-expression lhs env)
     (check-expression rhs env)]
    [(list 'disequality lhs "!=" rhs)
     (check-expression lhs env)
     (check-expression rhs env)]
    [(list 'inequality lhs (or "<" ">") rhs)
     (check-expression lhs env)
     (check-expression rhs env)]
    [(list 'predication term "is" adjective)
     (check-expression term env)]
    [(list 'predication (list 'json-pointer jp) "exists")
     env]
    [(list 'predication expr "has" "length" len)
     (check-expression expr env)
     (check-expression len env)]
    [else
     (error (format "Cannot check assertion ~a" ass))]))

(define/contract (check-import step env)
  (list? environment? . -> . environment?)
  (match step
    [(list 'filename (? string? f))
     (define p (build-path 'same f))
     (unless (file-exists? p)
       (error (format "No such file: ~a" (path->string p))))
     (define imported-program (load-riposte-file p))
     (unless (list? imported-program)
       (error (format "Cannot import ~a because it is malformed."
                      (path->string p))))
     (check-program imported-program env)]
    [else
     (error "Cannot handle import statement: ~a" step)]))

(define/contract (check-schema step env)
  (list? environment? . -> . environment?)
  (match step
    [(list 'schema-ref "in" (? string? filename))
     env]
    [else
     (error (format "Cannot make sense of schema step ~a" step))]))

(define/contract (check-step step env)
  (list? environment? . -> . environment?)
  (match step
    [(list 'program-step s)
     (match s
       [(list 'assignment ass)
        (check-assignment ass env)]
       [(list 'unset "unset" header)
        env]
       [(list 'import "import" import)
        (check-import import env)]
       [(list 'command cmd uri-like)
        (check-command cmd env)
        (check-uri uri-like env)
        env]
       [(list 'command cmd (list 'id id) uri-like)
        (check-command cmd env)
        (check-identifier id env)
        (check-uri uri-like env)
        env]
       [(list 'command cmd uri-like "with" "headers" h more)
        (match more
          [(list 'responds-with "responds" "with" code)
           (check-command cmd env)
           (check-uri uri-like env)
           (check-headers h env)
           (check-code code env)]
          [else
           (error (format "Cannot handle command ~a" s))])]
       [(list 'command cmd uri-like (list 'responds-with "responds" "with" code))
        (check-command cmd env)
        (check-uri uri-like env)
        (check-code code env)]
       [(list 'command cmd uri-like (list 'responds-with "responds" "with" code) "and" (list 'satisfies "satisfies" "schema" schema))
        (check-command cmd env)
        (check-uri uri-like env)
        (check-code code env)
        (check-schema schema env)]
       [(list 'command cmd (list 'id (list 'normal-identifier id)) uri-like "with" "headers" h (list 'responds-with "responds" "with" code))
        (check-command cmd env)
        (check-uri uri-like env)
        (check-headers h env)
        (check-code code env)]
       [(list 'command cmd (list 'id (list 'normal-identifier id)) uri-like more)
        (match more
          [(list 'responds-with "responds" "with" code)
           (check-command cmd env)
           (check-uri uri-like env)
           (check-code code env)]
          [(list "with" "headers" h (list 'responds-with "responds" "with" c))
           (check-command cmd env)
           (check-uri uri-like env)
           (check-headers h env)
           (check-code c env)]
          [else
           (error (format "Cannot handle command ~a" s))])]
       [(list 'assertion assertion)
        (check-assertion assertion env)]
       [else
        (error (format "Cannot handle step ~a" s))])]
    [else
     (error (format "Cannot handle step ~a" step))]))

(define/contract (check-steps steps environment)
  (list? environment? . -> . void)
  (cond ((empty? steps)
         environment)
        (else
         (check-steps (rest steps)
                      (check-step (first steps)
                                  environment)))))

(define/contract (check-program program [env (make-fresh-environment)])
  (->* (list?)
       (environment?)
       environment?)
  (match program
    [(list-rest 'riposte-program steps)
     (check-steps steps env)]
    [else
     (error (format "Cannot handle program ~a" program))]))
