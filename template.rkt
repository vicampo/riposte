#lang racket/base

(provide expand-uri-template)

(require racket/contract
         racket/match
         racket/list
         racket/string)

(module+ test
  (require rackunit))

(struct variable
  (operator name))

(struct uri-template
  (operator variables))

(define/contract (expand-uri-template/chars chars)
  ((listof char?) . -> . (listof (or/c (listof char?)
                                       uri-template?)))
  (match chars
    [(list)
     (list)]
    [(cons #\} more)
     (error "Malformed URI Template (found a dangling \"}\")")]
    [(cons #\{ more)
     (define i (index-of chars #\} char=?))
     (unless (exact-nonnegative-integer? i)
       (error "Malformed URI Template (found \"{\" without a closing \"}\")"))
     (define up-to-brace (take chars i))
     (define t (uri-template #f (string-split (list->string up-to-brace) ",")))
     (cons t (expand-uri-template/chars (drop chars (add1 i))))]
    [(list c)
     (list (list c))]
    [(cons c more)
     (define result (expand-uri-template/chars more))
     (when (empty? result)
       (error "Unexpected empty result!"))
     (define f (first result))
     (cond [(uri-template? f)
            (cons (list c)
                  result)]
           [else
            (cons (cons c f)
                  (rest result))])]))

(define/contract (expand-uri-template str)
  (string? . -> . (listof (or/c string? uri-template?)))
  (define (maybe-string-it x)
    (cond [(list? x)
           (list->string x)]
          [else
           x]))
  (define result (expand-uri-template/chars (string->list str)))
  (map maybe-string-it result))

(module+ test
  (let* ([t "https://example.com/{a}/"]
         [result (expand-uri-template t)])
    (check-= (length result)
             3
             0)
    (check-equal? (first result)
                  "https://example.com/")
    (check-true (uri-template? (second result)))
    (check-equal? (third result)
                  "/")))
