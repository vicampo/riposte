#lang racket/base

(provide make-json-object-expression
         make-json-array-expression)

(require racket/class
         racket/contract
         racket/match
         (only-in racket/list
                  empty?)
         (only-in racket/hash
                  hash-union)
         (only-in ejs
                  ejsexpr?
                  ejs-object?
                  ejs-array?)
         (only-in (file "expression.rkt")
                  expression?
                  expression%)
         (only-in (file "identifier.rkt")
                  identifier-expression?)
         (only-in (file "environment.rkt")
                  environment?))

(define json-expression%
  (class expression%
    (super-new)))

(define/contract (json-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x json-expression%)))

(define/contract (eval-object-item key value env)
  (string? (or/c ejsexpr? expression?) environment? . -> . ejs-object?)
  (hasheq (string->symbol key)
          (match value
            [(? ejsexpr?)
             value]
            [(? expression?)
             (send value evaluate env)])))

(define json-object-expression%
  (class json-expression%
    (super-new)
    (init-field items)
    (define/override (evaluate env)
      (cond [(empty? items)
             (hasheq)]
            [else
             (apply hash-union
                    (map (lambda (item)
                           (eval-object-item (car item)
                                             (cdr item)
                                             env))
                         items))]))
    (define/override (render)
      " OBJECT HERE! ")))

(define/contract (json-object-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x json-object-expression%)))

(define/contract (make-json-object-expression object-items)
  ((listof (cons/c string? (or/c ejsexpr? expression?))) . -> . json-object-expression?)
  (new json-object-expression%
       [items object-items]))

;; arrays

(define/contract (eval-array-item item env)
  ((or/c ejsexpr? expression?) environment? . -> . ejsexpr?)
  (match item
    [(? ejsexpr?)
     item]
    [(? expression?)
     (send item evaluate env)]))

(define json-array-expression%
  (class json-expression%
    (super-new)
    (init-field items)
    (define/override (evaluate env)
      (map (lambda (x) (eval-array-item x env))
           items))
    (define/override (render)
      " ARRAY HERE! ")))

(define/contract (json-array-expression? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x json-array-expression%)))

(define/contract (make-json-array-expression array-items)
  ((listof (or/c ejsexpr? expression?))
   . -> . json-array-expression?)
  (new json-array-expression%
       [items array-items]))
