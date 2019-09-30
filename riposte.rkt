#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         racket/match
         racket/pretty
         racket/format
         (only-in racket/list
                  empty?)
         (only-in racket/port
                  open-output-nowhere)
         racket/string
         dotenv
         misc1/syntax
         (file "util.rkt")
         (only-in (file "./version.rkt")
                  riposte-version)
         (prefix-in riposte: (file "reader.rkt"))
         riposte
         racket/contract)

(define/contract opt-version
  parameter?
  (make-parameter #f))

(define/contract opt-dotenvs
  parameter?
  (make-parameter (list)))

(define opt-lint (make-parameter #f))

(define/contract (default-error-handler e)
  (exn? . -> . any)
  (log-error "~a" (exn-message e))
  (exit 1))

(define-syntax (run! stx)
  (syntax-case stx ()
    [(_ #:error-handler handler steps ...)
     #'(with-handlers ([exn? handler])
         (begin0
             (void)
           `@(list steps ...)))]
    [(_ steps ...)
     #'(with-handlers ([exn? default-error-handler])
         (begin0
             (void)
           `@(list steps ...)))]))

(define/contract (check-dotenvs dotenvs)
  ((listof string?) . -> . void)
  (for ([f dotenvs])
    (define p (build-path 'same f))
    (unless (file-exists? p)
      (error "Cannot read environment file ~a" (path->string p))
      (exit 1))))

(define (run-repl)
  (current-read-interaction riposte:read-syntax-for-repl)
  (namespace-require 'riposte/expander)
  (read-eval-print-loop))

(module+ main

  (define file-to-process
    (command-line
     #:program "riposte"
     #:once-each
     [("--version") "Print the version"
                    (opt-version #t)]
     [("--lint") "Check syntax"
                 (opt-lint #t)]
     #:multi
     [("--env") f
                "Specify an environment file to use (can be specified multiple times)"
                (opt-dotenvs (cons f (opt-dotenvs)))]
     #:args args
     args))

  (when (opt-version)
    (displayln (format "~a" riposte-version))
    (exit 0))

  (match file-to-process
    [(list)
     (run-repl)]
    [(list filename)
     (unless (file-exists? filename)
       (displayln (format "No such file: ~a" filename)
                  (current-error-port))
       (exit 1))

     (run! (check-dotenvs (opt-dotenvs)))

     (run! (dotenv-load! (opt-dotenvs)))

     (with-handlers ([exn? (lambda (err)
                             (displayln (string-trim (exn-message err))
                                        (current-error-port))
                             (exit 1))])
       (dynamic-require (string->path filename)
                        (cond [(opt-lint) (void)]
                              [else #f])))]))
