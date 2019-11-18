#lang racket/base

(provide json-object?
         json-array?
         starts-with?
         ends-with?)

(require racket/match
         racket/contract
         (only-in racket/string
                  string-prefix?
                  string-suffix?)
         (only-in racket/list
                  empty?
                  list-prefix?)
         json)

(provide/contract
 [equal-jsexprs? (jsexpr? jsexpr? . -> . boolean?)])

(module+ test
  (require rackunit))

; it is assumed that x is a jsexpr? value
(define (json-object? x)
  (hash? x))

; it is assumed that x is a jsexpr? value
(define (json-array? x)
  (list? x))

(define (starts-with? jsexpr1 jsexpr2)
  (cond [(string? jsexpr1)
         (unless (string? jsexpr2)
           (error "First argument is a string, but the second argument is not."))
         (string-prefix? jsexpr1 jsexpr2)]
        [(list? jsexpr1)
         (unless (list? jsexpr2)
           (error "First argument is an array, but the second argument is not."))
         (list-prefix? jsexpr2 jsexpr1 equal-jsexprs?)]))

(module+ test
  (check-true (starts-with? "45" "4"))
  (check-true (starts-with? "45" ""))
  (check-false (starts-with? "45" " "))
  (check-true (starts-with? "45" "45"))
  (check-false (starts-with? (list) (list 4)))
  (check-true (starts-with? (list 4) (list)))
  (check-true (starts-with? (list 9 4) (list 9 4)))
  (check-false (starts-with? (list 6 4) (list 6 4 5))))

(define (ends-with? jsexpr1 jsexpr2)
  (cond [(string? jsexpr1)
         (unless (string? jsexpr2)
           (error "First argument is a string, but the second argument is not."))
         (string-suffix? jsexpr1 jsexpr2)]
        [(list? jsexpr1)
         (unless (list? jsexpr2)
           (error "First argument is an array, but the second argument is not."))
         (list-prefix? (reverse jsexpr2) (reverse jsexpr1) equal-jsexprs?)]))

(module+ test
  (check-true (ends-with? "hi!" "!"))
  (check-true (ends-with? "empty" ""))
  (check-false (ends-with? "" "empty"))
  (check-true (ends-with? "45" "5"))
  (check-false (ends-with? "45" "4"))
  (check-true (ends-with? (list 4 5) (list 5)))
  (check-true (ends-with? (list 4 5) (list 4 5)))
  (check-false (ends-with? (list 4 5) (list 3 4 5)))
  (check-false (ends-with? (list 4 5) (list 5 4)))
  (check-true (ends-with? (list 4 5) (list))))

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
