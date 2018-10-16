#lang racket/base

(provide eval-program)

(require (file "environment.rkt")
         (file "util.rkt")
         (file "program.rkt")
         (only-in (file "step.rkt")
                  step?)
         racket/contract
         racket/match
         (only-in racket/system
                  string-no-nuls?)
         (only-in racket/function
                  const)
         (only-in racket/list
                  empty?)
         (only-in ejs
                  equal-ejsexprs?)
         racket/class
         racket/hash
         brag/support
         net/url
         http
         json
         uri-template
         json-pointer
         argo)

(define (value? x)
  (jsexpr? x))

(define (extract-property property env)
  (match property
    [(list 'json-object-item (list 'json-object-property prop-expr) ":" val-expr)
     (define p (eval-expression prop-expr env))
     (define v (eval-expression val-expr env))
     (unless (string? p)
       (error "Evaluating expression ~a yielded a non-string: ~a" prop-expr p))
     (hash (string->symbol p)
           v)]))

(define (extract-properties properties env)
  (match properties
    [(list)
     (hasheq)]
    [(list-rest prop "," more)
     (hash-union (extract-property prop env)
                 (extract-properties more env))]
    [(list prop)
     (extract-property prop env)]))

(define/contract (eval-expression expr env)
  ((or/c string? list?) environment? . -> . value?)
  (match expr
    [(? string?)
     expr]
    [(list 'json-string s)
     s]
    [(list 'normal-identifier id)
     (lookup-variable id env)]
    [(list 'id identifier)
     (match identifier
       [(list 'env-identifier (? string? id))
        (lookup-environment-variable id)]
       [(list 'env-identifier (? string? id) "with" "fallback" fallback)
        (define v (getenv id))
        (match v
          [(? string-no-nuls?)
           v]
          [else
           (eval-expression fallback env)])]
       [(list 'normal-identifier id)
        (lookup-variable id env)])]
    [(list 'head-id identifier)
     (lookup-header identifier env)]
    [(list 'expression e)
     (eval-expression e env)]
    [(list 'expression x "*" y)
     (define x-val (eval-expression x env))
     (define y-val (eval-expression y env))
     (unless (number? x-val)
       (error (format "Expression ~a works out to ~a, which is not a number." x x-val)))
     (unless (number? y-val)
       (error (format "Expression ~a works out to ~a, which is not a number." y y-val)))
     (* x-val y-val)]
    [(list 'expression x "+" y)
     (define x-val (eval-expression x env))
     (define y-val (eval-expression y env))
     (unless (number? x-val)
       (error (format "Expression ~a works out to ~a, which is not a number." x x-val)))
     (unless (number? y-val)
       (error (format "Expression ~a works out to ~a, which is not a number." y y-val)))
     (+ x-val y-val)]
    [(list 'json-expression jsexpr)
     (eval-expression jsexpr env)]
    [(list 'json-string s)
     s]
    [(list 'json-pointer (? string? jp-expr))
     (define m (regexp-match #rx"^[$]([^/]+)(/.+)$" jp-expr))
     (match m
       [(list whatever id jp)
        (define e (eval-expression (list 'id (list 'normal-identifier id)) env))
        (json-pointer-value jp
                            e)]
       [else
        (json-pointer-value jp-expr
                            (environment-response env))])]
    [(list 'json-number (cons 'json-integer digits))
     (string->number (apply string-append
                            (map (lambda (x)
                                   (format "~a" x))
                                 digits)))]
    [(list 'json-object "{" properties ... "}")
     (define extracted (extract-properties properties env))
     extracted]
    [(list 'json-array "[" items ... "]")
     (map (lambda (i) (eval-expression i env))
          (remove "," items))]
    [(list 'json-array-item item)
     (eval-expression item env)]
    [else
     (error (format "Don't know how to evaluate expression ~a." expr))]))

(define/contract (value-satisfies-judgment? value judgment environment)
  (value? list? environment? . -> . boolean?)
  (match judgment
    [(list 'json-type (list 'json-number-type (list 'arithmetical-adjective adj) number-type))
     (match number-type
       ["integer"
        (unless (integer? value)
          (error (format "Value ~a should be an integer, but it isn't." value)))]
       [else
        (error (format "Cannot make sense of JSON number type \"~a\"." number-type))])
     (match adj
       ["positive"
        (unless (> value 0)
          (error (format "Value ~a should be positive, but it isn't." value)))]
       ["negative"
        (unless (< value 0)
          (error (format "Value ~a should be positive, but it isn't." value)))]
       [else
        (error (format "Don't know how to deal with arithmetical adjective \"~a." adj))])
     #t]
    [else
     (error (format "Cannot make sense of judgment ~a" judgment))]))

(define/contract (eval-assignment assign env)
  (list? environment? . -> . any)
  (match assign
    [(list 'header-assignment header ":=" expr)
     (unless (string? header)
       (error (format "Header ~a is not a string." header)))
     (extend-environment/header env
                                (string->symbol header)
                                (eval-expression expr env))]
    [(list 'normal-assignment (list 'id (list 'normal-identifier id)) ":=" expr)
     (extend-environment env
                         (string->symbol id)
                         (eval-expression expr env))]
    [(list 'normal-assignment (list 'id (list 'global-identifier identifier)) ":=" expr)
     (unless (string? identifier)
       (error (format "Global assignment identifier ~a is not a strong." identifier)))
     (extend-environment/global env
                                identifier
                                (eval-expression expr env))]
    [(list 'normal-assignment identifier ":=" expr "(" judgment ")")
     (let [(id (match identifier
                 [(list 'id (list 'normal-identifier id))
                  id]
                 [else
                  (error (format "Cannot make sense of identifier ~a" identifier))]))]
       (unless (string? id)
         (error (format "Normal assignment identifier ~a is not a string." id)))
       (define v (eval-expression expr env))
       (unless (value-satisfies-judgment? v judgment env)
         (error (format "The expression ~a, which evaluates to ~a, does not satisfy the judgment ~a."
                        expr
                        v
                        judgment)))
       (extend-environment env
                           (string->symbol id)
                           v))]
    [else
     (error (format "Cannot handle assignment ~a" assign))]))

(define (render-expression expr)
  (match expr
    [(list 'inequality lhs sign rhs)
     (format "~a ~a ~a"
             (render-expression lhs)
             sign
             (render-expression rhs))]
    [(list 'expression (list 'id (list 'normal-identifier (? string? id))))
     (format "$~a" id)]
    [else
     (error "Don't know how to render expression \"~a\"." expr)]))

(define (explain-failed-assertion response assertion env)
  (match assertion
    [(list 'inequality lhs sign rhs)
     (format "\"~a ~a ~a\" fails: the left-hand side is ~a, but the right-hand side is ~a."
             (render-expression lhs)
             sign
             (render-expression rhs)
             (eval-expression lhs env)
             (eval-expression rhs env))]
    [(list 'equality lhs "=" rhs)
     (format "\"~a = ~a\" fails: the left-hand side is ~a, but the right-hand side is ~a."
             (render-expression lhs)
             (render-expression rhs)
             (eval-expression lhs env)
             (eval-expression rhs env))]
    [else
     (error "Cannot explain failure of assertion ~a." assertion)]))

(define/contract (response-satisfies-assertion? response assertion env)
  (jsexpr? list? environment? . -> . boolean?)
  (match assertion
    [(list 'equality lhs "=" rhs)
     (define lhs-val (eval-expression lhs env))
     (define rhs-val (eval-expression rhs env))
     (equal-ejsexprs? lhs-val rhs-val)]
    [(list 'disequality lhs "!=" rhs)
     (define lhs-val (eval-expression lhs env))
     (define rhs-val (eval-expression rhs env))
     (not (equal-ejsexprs? lhs-val rhs-val))]
    [(list 'predication header-name "is" "absent")
     (not (hash-has-key? (environment-response-headers env)
                         (string->symbol header-name)))]
    [(list 'predication expr "is" (list 'json-type json-type))
     (define v (eval-expression expr env))
     (match json-type
       [(list 'json-sequence-type "array")
        (list? v)]
       [(list 'json-sequence-type (list 'sequence-adjective "empty") "array")
        (and (list? v)
             (empty? v))]
       [(list 'json-sequence-type (list 'sequence-adjective "non" "empty") "array")
        (and (list? v)
             (not (empty? v)))]
       [(list 'json-object-type (list 'object-adjective "empty") "object")
        (and (hash? v)
             (hash-empty? v))]
       [(list 'json-object-type (list 'object-adjective "non" "empty") "object")
        (and (hash? v)
             (not (hash-empty? v)))]
       [else
        (error (format "What kind of JSON type is ~a ?" json-type))])]
    [(list 'predication (list 'json-pointer jsp) "exists")
     (with-handlers ([exn:fail:contract? (const #f)])
       (begin0
           #t
         (eval-expression (list 'json-pointer jsp) env)))]
    [(list 'inequality lhs "<" rhs)
     (define lhs-val (eval-expression lhs env))
     (define rhs-val (eval-expression rhs env))
     (unless (number? lhs-val)
       (error (format "The expression ~a works out to ~a, which is not a number." lhs lhs-val)))
     (unless (number? rhs-val)
       (error (format "The expression ~a works out to ~a, which is not a number." rhs rhs-val)))
     (< lhs-val rhs-val)]
    [(list 'inequality lhs ">" rhs)
     (define lhs-val (eval-expression lhs env))
     (define rhs-val (eval-expression rhs env))
     (unless (number? lhs-val)
       (error (format "The expression ~a works out to ~a, which is not a number." lhs lhs-val)))
     (unless (number? rhs-val)
       (error (format "The expression ~a works out to ~a, which is not a number." rhs rhs-val)))
     (> lhs-val rhs-val)]
    [else
     (error (format "Cannot make sense of assertion ~a" assertion))]))

(define/contract (eval-step step env)
  (step? environment? . -> . environment?)
  (send step evaluate env))

(define/contract (eval-steps steps env)
  ((listof step?) environment? . -> . environment?)
  (foldl eval-step
         env
         steps))

(define/contract (eval-program program [env (make-fresh-environment)])
  (->*
   (program?)
   (environment?)
   environment?)
  (define/contract (eval step-or-steps env)
    ((or/c step? (listof step?)) environment? . -> . environment?)
    (cond [(step? step-or-steps)
           (eval-step step-or-steps env)]
          [(list? step-or-steps)
           (eval-steps step-or-steps env)]
          [else
           (error (format "WTF is ~a" step-or-steps))]))
  (foldl eval env program))
