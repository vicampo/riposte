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

(define sum%
  (class expression%
    (super-new)
    (init-field lhs rhs)
    (define/override (evaluate env)
      (define l (ensure-ejsexpr lhs env))
      (define r (ensure-ejsexpr rhs env))
      (unless (real? l)
        (error "Left-hand side of ~a is not a number!"
               (send this render)))
      (unless (real? r)
        (error "Right-hand side of ~a is not a number!"
               (send this render)))
      (+ l r))
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
