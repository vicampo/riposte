#lang racket/base

(provide #%module-begin
         #%app
         #%top
         #%datum
         riposte-program
         header-assignment
         parameter-assignment
         normal-assignment
         expression
         json-object
         json-object-item
         json-array
         command
         uri-template
         uri-template-expression
         uri-template-variable-list
         uri-template-varspec
         has-type
         json-pointer)

(require (file "expander.rkt"))

(module reader racket/base
  (provide (rename-out [riposte:read-syntax read-syntax]
                       [riposte:read read])
           get-info)

  (require (only-in (file "grammar.rkt")
                    parse)
           (only-in (file "tokenizer.rkt")
                    tokenize))

  (define (riposte:read-syntax name in)
    (define parse-tree (parse name (tokenize in)))
    (datum->syntax #f `(module anything riposte
                         ,parse-tree)))

  (define (riposte:read in)
    (syntax->datum (riposte:read-syntax 'src in)))

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(module-language)
         "expander.rkt"]
        [else default]))
    handle-query))
