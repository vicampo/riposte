#lang racket/base

(provide #%module-begin
         #%app
         #%top
         #%datum
         #%top-interaction
         riposte-program
         header-assignment
         parameter-assignment
         normal-assignment
         expression
         normal-identifier
         env-identifier
         json-object
         json-object-item
         json-array
         json-array-item
         json-boolean
         command
         uri-template
         uri-template-expression
         uri-template-variable-list
         uri-template-varspec
         has-type
         json-pointer
         unset
         schema-ref
         jp-existence
         equality
         inequality
         header-presence
         response-head-id
         sequence-predicate
         echo
         exec
         exec-arg-item
         riposte-repl)

(require (file "expander.rkt"))

(module reader racket/base
  (provide read-syntax
           read
           get-info)

  (require (file "reader.rkt"))

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(module-language)
         "expander.rkt"]
        [else default]))
    handle-query))
