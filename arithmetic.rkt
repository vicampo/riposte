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

(define/contract (duplicate-string str times)
  (string? exact-nonnegative-integer? . -> . string?)
  (cond [(= 0 times)
         ""]
        [(= 1 times)
         str]
        [else
         (format "~a~a" str (duplicate-string str (sub1 times)))]))

(module+ test
  (check-equal? ""
                (duplicate-string "french fries & gravy" 0))
  (check-equal? "french fries"
                (duplicate-string "french fries" 1))
  (check-equal? "abcabc"
                (duplicate-string "abc" 2)))

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
                   [(string? r)
                    (unless (positive? l)
                      (cond [(expression? l)
                             (error (format "Cannot add: ~a works out to ~a, which is not positive."
                                            (send lhs render)
                                            (render-ejsexprish l)))]
                            [else
                             (error (format "Cannot add: ~a is not positive." l))]))
                    (unless (exact-nonnegative-integer? l)
                      (cond [(expression? l)
                             (error (format "Cannot add: ~a works out to ~a, which is not an integer."
                                            (send lhs render)
                                            (render-ejsexprish l)))]
                            [else
                             (error (format "Cannot add: ~a is not an integer." l))]))
                    (duplicate-string r l)]
                   [else
                    (cond [(expression? l)
                           (error (format "Cannot add: ~a works out to ~a, which is neither a string nor an integer."
                                          (send lhs render)
                                          (render-ejsexprish l)))]
                          [else
                           (error (format "Cannot add: ~a is neither a string nor an integer"
                                          (render-ejsexprish l)))])])]
            [(string? l)
             (cond [(exact-nonnegative-integer? r)
                    (duplicate-string l r)]
                   [(string? r)
                    (format "~a~a" l r)]
                   [else
                    (cond [(expression? r)
                           (error (format "Cannot add: ~a works out to ~a, which is neither a string nor an integer."
                                          (send rhs render)
                                          (render-ejsexprish r)))]
                          [else
                           (error (format "Cannot add: ~a is neither a string nor an integer"
                                          (render-ejsexprish r)))])])]
            [else
             (cond [(expression? l)
                    (error (format "Cannot add: ~a works out to ~a, which is neither a string nor an integer."
                                   (send lhs render)
                                   (render-ejsexprish l)))]
                   [else
                    (error (format "Cannot add: ~a is neither a string nor an integer"
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
