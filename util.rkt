#lang racket/base

(provide load-riposte-file
         ejsexpr->jsexpr
         jsexpr->ejsexpr
         ensure-ejsexpr
         render-ejsexprish)

(require racket/function
         racket/contract
         racket/class
         brag/support
         (only-in json
                  jsexpr?)
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string)
         (file "grammar.rkt")
         (file "tokenizer.rkt")
         (only-in (file "expression.rkt")
                  expression?)
         (only-in (file "environment.rkt")
                  environment?))

(define/contract (load-riposte-file f)
  (path-string? . -> . (or/c false/c list?))
  (with-handlers ([exn:fail:parsing? (const #f)])
    (parse-to-datum (tokenize-file f))))

;; converting ejsexprs to jsexprs

(define/contract (ejsexpr->jsexpr x)
  (ejsexpr? . -> . jsexpr?)
  (cond [(symbol? x)
         ;; 'null
         x]
        [(boolean? x)
         x]
        [(string? x)
         x]
        [(number? x)
         (cond [(integer? x)
                x]
               [else
                (exact->inexact x)])]
        [(list? x)
         (map ejsexpr->jsexpr x)]
        [(hash? x)
         (make-hasheq (map (lambda (pair)
                             (cons (car pair)
                                   (ejsexpr->jsexpr (cdr pair))))
                           (hash->list x)))]))

(define/contract (jsexpr->ejsexpr x)
  (jsexpr? . -> . ejsexpr?)
  (cond [(string? x)
         x]
        [(boolean? x)
         x]
        [(number? x)
         (cond [(exact? x)
                x]
               [else
                (inexact->exact x)])]
        [(list? x)
         (map jsexpr->ejsexpr x)]
        [(hash? x)
         (make-hasheq (map (lambda (pair)
                             (cons (car pair)
                                   (jsexpr->ejsexpr (cdr pair))))
                           (hash->list x)))]
        [else
         'null]))

(define/contract (ensure-ejsexpr thing env)
  ((or/c ejsexpr? expression?) environment? . -> . ejsexpr?)
  (cond [(expression? thing)
         (send thing evaluate env)]
        [else
         thing]))

(define/contract (render-ejsexprish thing)
  ((or/c ejsexpr? expression?) . -> . string?)
  (cond [(expression? thing)
         (send thing render)]
        [else
         (ejsexpr->string thing)]))
