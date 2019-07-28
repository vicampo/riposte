#lang racket/base

(provide ejsexpr->jsexpr
         jsexpr->ejsexpr
         render-ejsexprish
         bytes->string
         port->chars)

(require racket/function
         racket/contract
         racket/class
         racket/match
         brag/support
         (only-in json
                  jsexpr?)
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string))

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

(define/contract (render-ejsexprish thing)
  ((or/c ejsexpr? expression?) . -> . string?)
  (cond [(expression? thing)
         (send thing render)]
        [else
         (ejsexpr->string thing)]))

(define (bytes->string bstr)
  (define (fail err) #f)
  (with-handlers ([exn:fail:contract? fail])
    (bytes->string/utf-8 bstr)))

(define/contract (port->chars ip)
  (input-port? . -> . (listof char?))
  (match (read-char ip)
    [(? eof-object?)
     (list)]
    [(? char? c)
     (cons c (port->chars ip))]))
