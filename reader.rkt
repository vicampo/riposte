#lang racket/base

(provide read-syntax
         read
         read-syntax-for-repl
         read-for-repl)

(require (file "grammar.rkt")
         (file "tokenizer.rkt")
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
               [(eq? 'exec (syntax->datum (stx-car stx)))
                (define datum (syntax->datum stx))
                (define full-path (build-path cwd
                                              (syntax->datum (car (stx-cdr stx)))))
                (datum->syntax #f (append (list (car datum)
                                                (path->string full-path))
                                          (cddr datum)))]
               [else
                stx])]
        [else
         stx]))

(define (read-syntax name in)
  (define-values (dir base is-directory?)
    (cond [(path-string? name)
           (split-path name)]
          [else
           (values #f #f #f)]))

  (define cwd
    (cond [(path? dir)
           dir]
          [else
           (current-directory)]))

  (param-cwd cwd)

  (define parse-tree (parse name (tokenize in)))
  (define imports-expanded (expand-imports parse-tree cwd))
  (datum->syntax #f `(module anything riposte
                       ,imports-expanded)))

(define (read in)
  (syntax->datum (read-syntax 'src in)))

(define (read-for-repl in)
  (syntax->datum (read-syntax-for-repl 'repl in)))

(define repl-rule (make-rule-parser riposte-repl))

(define (read-syntax-for-repl name in)
  (define l (read-line in))
  (match l
    [(? eof-object?)
     eof]
    [(? string? s)
     (repl-rule (tokenize s))]))
