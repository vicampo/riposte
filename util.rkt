#lang racket/base

(provide ;ejsexpr->jsexpr
         ;jsexpr->ejsexpr
         ;render-ejsexprish
         bytes->string
         port->chars
         file-content/bytes
         hash-remove*
         comment-out-lines)

(require racket/function
         racket/contract
         racket/class
         racket/match
         racket/port
         (only-in racket/list
                  empty?
                  add-between)
         (only-in racket/string
                  string-split)
         (only-in racket/format
                  ~a)
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

(define (hash-remove* h1 keys)
  (match keys
    ['() h1]
    [(cons k ks)
     (hash-remove* (hash-remove h1 k) ks)]))

(define (comment-out-line s)
  (string-append "# " s))

(define (comment-out-lines str)
  (define nl (~a #\newline))
  (define lines (string-split str nl))
  (apply string-append
         (add-between (map comment-out-line lines) nl)))
