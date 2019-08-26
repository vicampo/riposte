#lang racket/base

(provide read-syntax)

(require (only-in (file "tokenizer.rkt")
                  tokenize)
         (only-in (file "grammar.rkt")
                  parse))

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (datum->syntax
   #f
   `(module riposte-module "expander.rkt"
      ,parse-tree)))
