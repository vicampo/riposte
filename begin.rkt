#lang racket/base

(provide block-expression?
         make-block-expression)

(require racket/class
         racket/contract
         (file "expression.rkt")
         (file "step.rkt")
         (file "environment.rkt"))

(define block-expression%
  (class expression%
    (super-new)
    (init-field steps)
    (define/override (evaluate env)
      (define/contract (f step env)
        (step? environment? . -> . environment?)
        (send step evaluate env))
      (foldl f
             env
             steps))
    (define/override (render)
      (apply string-append
             (map (lambda (step)
                    (format "~a~%" (send step render)))
                  steps)))))

(define (block-expression? x)
  (and (object? x)
       (is-a? x block-expression%)))

(define/contract (make-block-expression block-steps)
  ((listof step?) . -> . block-expression?)
  (new block-expression%
       [steps block-steps]))
