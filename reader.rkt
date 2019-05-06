#lang br/quicklang

(provide read-syntax
         clean-parse-tree
         #;simplify-imports-for-dir
         flatten-imported-scripts)

(require racket/contract
         (only-in (file "tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "grammar.rkt")
                  parse)
         (only-in (file "parameters.rkt")
                  param-cwd)
         syntax/stx)

(define/contract (clean-parse-tree tree)
  (syntax? . -> . syntax?)
  (cond [(stx-list? tree)
         (define terms (map clean-parse-tree
                            (syntax->list tree)))
         (datum->syntax tree
                        (filter (lambda (a)
                                  (let [(d (syntax->datum a))]
                                    (or (not (char? d))
                                        (not (char-whitespace? d)))))
                                terms))]
        [else
         tree]))

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
                  (parse path (make-tokenizer (open-input-file path #:mode 'text)))))]
               [else
                (datum->syntax tree
                               (stx-map expand-imports tree))])]
        [else
         tree]))

(define productions-to-simplify
  '(uri-template-literals
    http-response-code
    responds-with
    import-filename))

(define/contract (simplify tree)
  (syntax? . -> . syntax?)
  (cond [(stx-pair? tree)
         (define head (stx-car tree))
         (define head/symbol (syntax->datum head))
         (cond [(list? (member head/symbol productions-to-simplify eq?))
                (define letters (stx-cdr tree))
                (define to-import (apply string-append (map syntax->datum letters)))
                (datum->syntax tree
                               (list head/symbol to-import))]
               [else
                (datum->syntax tree
                               (stx-map simplify
                                        tree))])]
        [else
         tree]))

(define/contract (accumulate-children syntaxes)
  ((listof syntax?) . -> . (listof syntax?))
  (cond [(empty? syntaxes)
         (list)]
        [(stx-list? (first syntaxes))
         (define h (stx-car (first syntaxes)))
         (cond [(eq? (syntax->datum h) 'riposte-program)
                (append (stx-cdr (first syntaxes))
                        (accumulate-children (rest syntaxes)))]
               [else
                (cons (first syntaxes)
                      (accumulate-children (rest syntaxes)))])]
        [else
         (cons (first syntaxes)
               (accumulate-children (rest syntaxes)))]))

(define/contract (push-toplevels-up tree)
  (syntax? . -> . syntax?)
  (cond [(stx-pair? tree)
         (define children (accumulate-children (stx->list tree)))
         #`(#,@children)]
        [else
         tree]))

(define/contract (stx-cadr stx)
  (syntax? . -> . syntax?)
  (stx-car (stx-car (stx-cdr stx))))

(define (flatten-imported-scripts tree)
  (define pushed (push-toplevels-up tree))
  #`(riposte-program #,@pushed))

(define (read-syntax path port)
  (define parse-tree
    (parse path (make-tokenizer port)))
  (define simplified-templates
    (expand-imports (simplify parse-tree)))
  (define flattened (flatten-imported-scripts simplified-templates))
  (define pushed-up (push-toplevels-up flattened))
  (define module-datum
    `(module riposte-module riposte/expander
       ,pushed-up))
  (datum->syntax #f module-datum))
