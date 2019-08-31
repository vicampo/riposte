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
         equality)

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
           syntax/stx
           racket/pretty
           racket/match)

  (define (check-environment-variables thing)
    (match thing
      [(list 'env-identifier (? string? id))
       (match (getenv id)
         [#f (error (format "Environment variable \"~a\" undefined." id))]
         [else #t])]
      [(or (? symbol?)
           (? string?)
           (? number?)
           (? boolean?))
       #t]
      [(? list?)
       (for ([x thing])
         (check-environment-variables x))]))

  (define (expand-imports stx cwd)
    (cond [(stx-pair? stx)
           (cond [(eq? 'riposte-program (syntax->datum (stx-car stx)))
                  (define mapped (stx-map (lambda (s) (expand-imports s cwd))
                                          (stx-cdr stx)))
                  #`(riposte-program #,@mapped)]
                 [(eq? 'import (syntax->datum (stx-car stx)))
                  (define full-path (build-path cwd
                                                (syntax->datum (car (stx-cdr stx)))))
                  (define-values (next-dir next-base is-directory?)
                    (split-path full-path))
                  (define next-parse-tree
                    (call-with-input-file* full-path
                      (lambda (in)
                        (parse full-path (tokenize in)))))
                  (expand-imports next-parse-tree next-dir)]
                 [else
                  stx])]
          [else
           stx]))

  (define (riposte:read-syntax name in)
    (define-values (dir base is-directory?)
      (split-path name))

    (define cwd
      (cond [(path? dir)
             dir]
            [else
             (current-directory)]))

    (param-cwd cwd)

    (define parse-tree (parse name (tokenize in)))
    (define imports-expanded (expand-imports parse-tree cwd))
    (check-environment-variables (syntax->datum imports-expanded))
    (datum->syntax #f `(module anything riposte
                         ,imports-expanded)))

  (define (riposte:read in)
    (syntax->datum (riposte:read-syntax 'src in)))

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(module-language)
         "expander.rkt"]
        [else default]))
    handle-query))
