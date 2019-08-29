#lang racket/base

(provide expand-paths)

(require syntax/parse
         syntax/stx)

(module+ test
  (require rackunit))

(define (expand-paths thing base-dir)
  (log-error "Expanding ~a" thing)
  (define (expand-it thing)
    (syntax-parse thing
      [(schema-ref "in" path:string)
       (define new-path (build-path base-dir (syntax->datum #'path)))
       #`(schema-ref "in" #,(path->string new-path))]
      [i:id
       #'i]
      [s:string
       #'s]
      [n:number
       #'n]
      [(h t ...)
       #`(#,@(stx-map (lambda (x)
                        (expand-it x))
                      thing))]))
  (begin0
      (expand-it thing)
    (log-error "done.")))

(module+ test
  (check-equal? (syntax->datum (expand-paths #'(riposte-program) "foo"))
                (syntax->datum #'(riposte-program)))
  (check-equal? (syntax->datum (expand-paths #'(command
                                        "POST"
                                        (normal-identifier "payload")
                                        (uri-template "api/flub")
                                        (positive-satisfies (schema-ref "in" "schema.json")))
                                     "examples"))
                (syntax->datum #'(command
                                  "POST"
                                  (normal-identifier "payload")
                                  (uri-template "api/flub")
                                  (positive-satisfies (schema-ref "in" "examples/schema.json")))))
  (check-equal? (syntax->datum (expand-paths #'(json-object-item "b" 3) "bar"))
                (syntax->datum #'(json-object-item "b" 3))))
