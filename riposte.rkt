#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         racket/pretty
         racket/format
         (only-in racket/port
                  open-output-nowhere)
         dotenv
         (file "util.rkt")
         (only-in (file "./version.rkt")
                  riposte-version)
         (file "grammar.rkt")
         (only-in (file "tokenizer.rkt")
                  tokenize)
         racket/contract
         brag/support
         racket/match
         (file "./setup.rkt"))

(define/contract opt-version
  parameter?
  (make-parameter #f))

(define/contract opt-dotenvs
  parameter?
  (make-parameter (list)))

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

(define (fail-program e)
  (displayln (format "FAIL: ~a" (exn-message e)))
  (exit 1))

(define/contract (expand-imports program cwd)
  (any/c path? . -> . any/c)
  (match program
    [(list)
     (list)]
    [(or (? string?) (? number?) (? boolean?))
     program]
    [(list 'import (? string? filename))
     (define path (build-path cwd filename))
     (unless (file-exists? path)
       (error (format "Cannot import program at \"~a\": no such file." (path->string path))))
     (define-values (dir base is-directory?)
       (split-path path))
     (define tokens (tokenize path))
     (define parse-tree (parse tokens))
     (expand-imports (syntax->datum parse-tree) dir)]
    [(list 'schema-ref "in" (? string? filename))
     (list 'schema-ref "in" (path->string (build-path cwd filename)))]
    [(cons (? symbol? x) y)
     (cons x (map (lambda (s)
                    (expand-imports s cwd))
                  y))]))

(module+ main

  (define (complain-about-undefined-var err)
    (displayln (format "Undefined identifier \"~a\"." (exn:fail:contract:variable-id err))))

  (define (cancelling-prompt)
    (define (resume e)
      (displayln "")
      (cancelling-prompt))
    (with-handlers ([exn:break? resume])
      (read-eval-print-loop)))

  (define file-to-process
    (command-line
     #:program "riposte"
     #:once-each
     [("--version") "Print the version"
                    (opt-version #t)]
     #:multi
     [("--env") f
                "Specify an environment file to use (can be specified multiple times)"
                (opt-dotenvs (cons f (opt-dotenvs)))]
     #:args (args)
     args))

  (when (opt-version)
    (displayln (format "~a" riposte-version))
    (exit 0))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (define-values (dir base is-directory?)
    (split-path file-to-process))

  (when is-directory?
    (displayln (format "Given file is actually a directory: ~a" file-to-process))
    (exit 1))

  (run! (check-dotenvs (opt-dotenvs)))

  (run! (dotenv-load! (opt-dotenvs)))

  (define cwd
    (cond [(path? dir)
           dir]
          [else
           (current-directory)]))

  (dynamic-require (string->path file-to-process) #f))
