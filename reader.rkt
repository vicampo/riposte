#lang br/quicklang

(provide read-syntax)

(require racket/contract
         (only-in (file "new-tokenizer.rkt")
                  tokenize)
         (only-in (file "grammar.rkt")
                  parse)
         (only-in (file "parameters.rkt")
                  param-cwd)
         syntax/stx)

#;
(define/contract (expand-imports tree)
  (syntax? . -> . syntax?)
  (cond [(stx-pair? tree)
         (define head (stx-car tree))
         (cond [(eq? (syntax->datum head) 'import)
                (define to-import (cadr (syntax->datum (first (stx-cdr tree)))))
                (define cwd
                  (match (param-cwd)
                    [(? path?)
                     (param-cwd)]
                    [else
                     'same]))
                (define path (build-path cwd to-import))
                (expand-imports
                 (simplify
                  (parse path (tokenize path))))]
               [else
                (datum->syntax tree
                               (stx-map expand-imports tree))])]
        [else
         tree]))

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (datum->syntax
   #f
   `(module riposte-module riposte/expander
      ,parse-tree)))
