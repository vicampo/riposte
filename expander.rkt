#lang br/quicklang

(require racket/contract
         (file "evaluator.rkt")
         (only-in (file "environment.rkt")
                  make-fresh-environment
                  environment?)
         (only-in (file "assignment.rkt")
                  make-normal-assignment
                  make-header-assignment
                  make-parameter-assignment)
         (only-in (file "command.rkt")
                  make-command-expression)
         (only-in (file "json-pointer.rkt")
                  make-json-pointer-expression)
         (only-in (file "identifier.rkt")
                  make-variable-identifier-expression
                  make-parameter-identifier-expression
                  make-header-identifier-expression
                  make-environment-variable-identifier-expression)
         (only-in (file "json.rkt")
                  make-json-object-expression)
         (only-in (file "assertion.rkt")
                  make-response-code-matches-expression)
         (only-in (file "import.rkt")
                  make-import)
         (only-in (file "parameters.rkt")
                  param-environment)
         (only-in (file "step.rkt")
                              step?))

(require (for-syntax (only-in (file "grammar.rkt")
                              parse)
                     (only-in (file "tokenizer.rkt")
                              make-tokenizer)))

(define-macro (riposte-module-begin PARSE-TREE)
  #'(#%module-begin
     (eval-program PARSE-TREE
                   (param-environment))))

(provide (rename-out [riposte-module-begin #%module-begin]))

(define-macro (riposte-program STEPS ...)
  #'(list STEPS ...))

(provide riposte-program)

(define-macro (program-step STEP)
  #'STEP)

(provide program-step)

(define-macro-cases assignment
  [(assignment (normal-assignment ID ":=" VAL))
   #'(make-normal-assignment ID VAL)]
  [(assignment (header-assignment HEADER ":=" VAL))
   #'(make-header-assignment HEADER VAL)]
  [(assignment (parameter-assignment PARAM ":=" VAL))
   #'(make-parameter-assignment PARAM VAL)])

(provide assignment)

(define-macro (expression EXPR)
  #'EXPR)

(provide expression)

(define-macro (json-expression JSEXPR)
  #'JSEXPR)

(provide json-expression)

(define-macro-cases json-boolean
  ["true"
   #t]
  ["false"
   #t])

(provide json-boolean)

(define-macro (json-object "{" ITEMS ... "}")
  #'(make-json-object-expression (remove "," (list ITEMS ...))))

(provide json-object)

(define-macro (json-object-item PROP ":" EXPR-OR-ID)
  #'(cons PROP EXPR-OR-ID))

(provide json-object-item)

(define-macro (json-object-property PROP)
  #'PROP)

(provide json-object-property)

(define-macro (json-string S)
  #'S)

(provide json-string)

(define-macro-cases id
  [(id (normal-identifier X))
   #'(make-variable-identifier-expression X)]
  [(id (env-identifier X))
   #'(make-environment-variable-identifier-expression X)]
  [(id (env-identifier X "with" "fallback" F))
   #'(make-environment-variable-identifier-expression X #:fallback F)])

(provide id)

(define-macro (parameter-identifier PARAM)
  #'(make-parameter-identifier-expression PARAM))

(provide parameter-identifier)

(define-macro (normal-identifier ID)
  #'(make-variable-identifier-expression ID))

(provide normal-identifier)

(define-macro (head-id ID)
  #'(make-header-identifier-expression ID))

(provide head-id)

(define-macro-cases command
  [(command METHOD URI)
   #'(make-command-expression METHOD URI)]
  [(command METHOD URI "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI)
           (make-schema-assertion (make-response-expression)
                                  SCHEMA))]
  [(command METHOD URI "responds" "with" CODE)
   #'(list (make-command-expression METHOD URI)
           (make-response-code-matches-expression CODE))]
  [(command METHOD URI "responds" "with" CODE "and" "satisfies" SCHEMA)
   #'(list (make-command-expression METHOD URI)
           (make-response-code-matches-expression CODE)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD URI "with" "headers" HEADERS)
   #'(make-command-expression METHOD URI #:headers HEADERS)]
  [(command METHOD URI "with" "headers" HEADERS "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:headers HEADERS)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD URI "with" "headers" HEADERS "responds" "with" CODE)
   #'(list (make-command-expression METHOD URI)
           (make-response-code-matches-expression CODE))]
  [(command METHOD URI "with" "headers" HEADERS "responds" "with" CODE "and" "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:headers HEADERS)
           (make-response-code-matches-expression CODE)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD PAYLOAD URI)
   #'(make-command-expression METHOD URI #:payload PAYLOAD)]
  [(command METHOD PAYLOAD URI "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD PAYLOAD URI "responds" "with" CODE)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD)
           (make-response-code-matches-expression CODE))]
  [(command METHOD PAYLOAD URI "responds" "with" CODE "and" "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD)
           (make-response-code-matches-expression CODE)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD PAYLOAD URI "with" "headers" HEADERS)
   #'(make-command-expression METHOD URI #:payload PAYLOAD #:headers HEADERS)]
  [(command METHOD PAYLOAD URI "with" "headers" HEADERS "and" "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD #:headers HEADERS)
           (make-satisfies-schema-expression SCHEMA))]
  [(command METHOD PAYLOAD URI "with" "headers" HEADERS "responds" "with" CODE)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD #:headers HEADERS)
           (make-response-code-matches-expression CODE))]
  [(command METHOD PAYLOAD URI "with" "headers" HEADERS "responds" "with" CODE "and" "satisfies" "schema" SCHEMA)
   #'(list (make-command-expression METHOD URI #:payload PAYLOAD #:headers HEADERS)
           (make-response-code-matches-expression CODE)
           (make-satisfies-schema-expression SCHEMA))])

(provide command)

(define-macro-cases http-response-code
  [(http-response-code DIGIT-1 DIGIT-2 DIGIT-3)
   (format "~a~a~a"
           #'DIGIT-1
           #'DIGIT-2
           #'DIGIT-3)]
  [(http-response-code CODE)
   #'CODE])

(provide http-response-code)

(define-macro (json-pointer JP)
  #'(make-json-pointer-expression JP))

(provide json-pointer)

(define-macro (import "import" FILE)
  (define port (open-input-file (syntax->datum #'FILE)))
  (define path (build-path 'same (syntax->datum #'FILE)))
  (define parse-tree (parse path (make-tokenizer port)))
  (define parse-datum (syntax->datum parse-tree))
  (datum->syntax #'FILE
                   parse-datum))

(provide import)
