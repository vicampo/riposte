#lang racket/base

(provide ;ejsexpr->jsexpr
         ;jsexpr->ejsexpr
         ;render-ejsexprish
         bytes->string
         port->chars
         equal-jsexprs?
         file-content/bytes)

(require racket/function
         racket/contract
         racket/class
         racket/match
         racket/port
         (only-in racket/list
                  empty?)
         brag/support
         (only-in json
                  jsexpr?)
         #;
         (only-in ejs
                  ejsexpr?
                  ejsexpr->string))

(define (file-content/bytes path)
  (let ([p (open-input-file path)])
    (begin0
        (port->bytes p)
      (close-input-port p))))

#;
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

#;
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

#;
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

(define (equal-arrays? jsarr1 jsarr2)
  (cond [(empty? jsarr1)
         (empty? jsarr2)]
        [(empty? jsarr2)
         #f]
        [else
         (equal-jsexprs? (car jsarr1) (car jsarr2))]))

(define (equal-value-for-property? prop jsobj1 jsobj2)
  (and (hash-has-key? jsobj1 prop)
       (hash-has-key? jsobj2 prop)
       (equal-jsexprs? (hash-ref jsobj1 prop)
                       (hash-ref jsobj2 prop))))

(define (equal-objects? jsobj1 jsobj2)
  (define keys1 (hash-keys jsobj1))
  (define keys2 (hash-keys jsobj2))
  (and (equal? keys1 keys2)
       (andmap (lambda (k)
                 (equal-value-for-property? k jsobj1 jsobj2))
               keys1)))

(define (equal-jsexprs? jsexpr1 jsexpr2)
  (match jsexpr1
    ['null
     (eq? jsexpr2 'null)]
    [#t
     (eq? jsexpr2 #t)]
    [#f
     (eq? jsexpr2 #f)]
    [(? string?)
     (equal? jsexpr2 jsexpr1)]
    [(? number?)
     (and (number? jsexpr2)
          (= jsexpr1 jsexpr2))]
    [(? list?)
     (and (list? jsexpr2)
          (equal-arrays? jsexpr1 jsexpr2))]
    [(? hash?)
     (and (hash? jsexpr2)
          (equal-objects? jsexpr1 jsexpr2))]))
