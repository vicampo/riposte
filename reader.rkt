#lang br/quicklang

(provide read-syntax)

(require racket/contract
         (only-in (file "tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "grammar.rkt")
                  parse))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module riposte-module riposte/expander ,parse-tree))
  (datum->syntax #f module-datum))
