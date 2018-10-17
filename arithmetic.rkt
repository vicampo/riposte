#lang racket/base

(provide make-sum)

(require racket/class
         racket/contract
         (only-in ejs
                  ejsexpr?)
         (only-in (file "expression.rkt")
                  expression%
                  expression?)
         (only-in (file "util.rkt")
                  ensure-ejsexpr
                  render-ejsexprish))

(module+ test
  (require rackunit))

(define sum%
  (class expression%
    (super-new)
    (init-field lhs rhs)
    (define/override (evaluate env)
      (define l (ensure-ejsexpr lhs env))
      (define r (ensure-ejsexpr rhs env))
      (cond [(real? l)
             (cond [(real? r)
                    (+ l r)]
                   [else
                    (error "Cannot add: the left-hand side is a number, but the right-hand side isn't.")])]
            [(string? l)
             (cond [(string? r)
                    (format "~a~a" l r)]
                   [else
                    (error "Cannot add: the left-hand side is a number, but the right-hand side isn't.")])]
            [else
             (cond [(expression? l)
                    (error (format "Cannot add: ~a works out to ~a, which is neither a string nor a number."
                                   (send lhs render)
                                   (render-ejsexprish l)))]
                   [else
                    (error (format "Cannot add: ~a is neither a string nor a number."
                                   (render-ejsexprish l)))])]))
    (define/override (render)
      (define l (render-ejsexprish lhs))
      (define r (render-ejsexprish rhs))
      (format "~a + ~a" l r))))

(define/contract (sum? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x sum%)))

(define/contract (make-sum lhs rhs)
  ((or/c ejsexpr? expression?) (or/c ejsexpr? expression?) . -> . sum?)
  (new sum%
       [lhs lhs]
       [rhs rhs]))
