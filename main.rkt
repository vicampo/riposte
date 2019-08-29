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
         normal-identifier
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
         schema-ref)

(require (file "expander.rkt"))

(module reader racket/base
  (provide (rename-out [riposte:read-syntax read-syntax]
                       [riposte:read read])
           get-info)

  (require (only-in (file "grammar.rkt")
                    parse)
           (only-in (file "tokenizer.rkt")
                    tokenize)
           (file "parameters.rkt")
           syntax/parse
           syntax/stx)

  (define (riposte:read-syntax name in)
    (displayln (format "Looking at syntax in ~a" name))

    (define-values (dir base is-directory?)
      (split-path name))

    (define cwd
      (cond [(path? dir)
             dir]
            [else
             (current-directory)]))

    (param-cwd cwd)

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
