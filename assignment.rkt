#lang racket/base

(provide make-header-assignment
         make-normal-assignment
         make-parameter-assignment
         assignment?)

(require racket/class
         racket/contract
         racket/match
         ejs
         (file "step.rkt")
         (file "environment.rkt")
         (only-in (file "identifier.rkt")
                  identifier-expression?
                  parameter-identifier-expression?
                  header-identifier-expression?)
         (only-in (file "expression.rkt")
                  expression?)
         (only-in (file "util.rkt")
                  ensure-ejsexpr
                  render-ejsexprish))

(define assignment%
  (class step%
    (super-new)
    (init-field lhs
                rhs)))

(define (assignment? x)
  (and (object? x)
       (is-a? x assignment%)))

(define header-assignment%
  (class assignment%
    (super-new)
    (inherit-field lhs rhs)
    (define/override (evaluate env)
      (log-error "evaluating a header assignment: rhs is ~a" rhs)
      (extend-environment/header env
                                 (string->symbol (send lhs get-name))
                                 (ensure-ejsexpr rhs env)))
    (define/override (render)
      (format "^~a := ~a"
              lhs
              (render-ejsexprish rhs)))))

(define/contract (header-assignment? x)
  (any/c . -> . boolean?)
  (and (assignment? x)
       (is-a? x header-assignment%)))

(define/contract (make-header-assignment header value)
  (header-identifier-expression? (or/c ejsexpr? expression?)
                                 . -> . header-assignment?)
  (new header-assignment%
       [lhs header]
       [rhs value]))

(define normal-assignment%
  (class assignment%
    (super-new)
    (inherit-field lhs rhs)
    (define/override (evaluate env)
      (extend-environment env
                          (string->symbol (send lhs get-name))
                          (ensure-ejsexpr rhs env)))
    (define/override (render)
      (format "~a := ~a"
              (send lhs render)
              (render-ejsexprish rhs)))))

(define/contract (normal-assignment? x)
  (any/c . -> . boolean?)
  (and (assignment? x)
       (is-a? x normal-assignment%)))

(define/contract (make-normal-assignment var value)
  (identifier-expression? (or/c ejsexpr? expression?) . -> . normal-assignment?)
  (new normal-assignment%
       [lhs var]
       [rhs value]))

(define parameter-assignment%
  (class assignment%
    (super-new)
    (inherit-field lhs rhs)
    (define/override (evaluate env)
      (log-error "evaluating an assignment: ~a to ~a" rhs lhs)
      (extend-environment/global env
                                 (send lhs get-name)
                                 (ensure-ejsexpr rhs env)))
    (define/override (render)
      (format "~a := ~a"
              (send lhs render)
              (render-ejsexprish rhs)))))

(define/contract (parameter-assignment? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x parameter-assignment%)))

(define/contract (make-parameter-assignment id val)
  (parameter-identifier-expression?
   (or/c ejsexpr? expression?) . -> .
   parameter-assignment?)
  (new parameter-assignment%
       [lhs id]
       [rhs val]))
