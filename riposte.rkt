#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         racket/pretty
         racket/format
         dotenv
         (file "util.rkt")
         (only-in (file "evaluator.rkt")
                  eval-program)
         (only-in (file "environment.rkt")
                  environment?
                  make-fresh-environment)
         (file "check.rkt")
         (only-in (file "program.rkt")
                  file->program)
         (only-in (file "parameters.rkt")
                  param-cwd)
         (only-in (file "./version.rkt")
                  riposte-version)
         (only-in (file "reader.rkt")
                  read-syntax)
         (file "grammar.rkt")
         (only-in (file "new-tokenizer.rkt")
                  tokenize)
         racket/contract
         brag/support
         racket/match
         (file "./setup.rkt"))

(define/contract opt-version
  parameter?
  (make-parameter #f))

(define/contract opt-lint
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

(define/contract (eval-program-in-dir dir file-to-process)
  ((or/c path? (one-of/c 'relative)) path? . -> . environment?)
  (define cwd
    (cond [(path? dir)
           dir]
          [else
           (current-directory)]))
  (parameterize ([param-cwd cwd]
                 [current-namespace (make-base-empty-namespace)])
    (namespace-require 'riposte/expander)
    (load file-to-process)))

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

(define (run-program filename cwd)
  (define parsed (syntax->datum (parse (tokenize filename))))
  (define expanded (expand-imports parsed cwd))
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require '(file "./expander.rkt"))
    (eval expanded)))

(module+ main

  (define (complain-about-undefined-var err)
    (displayln (format "Undefined identifier \"~a\"." (exn:fail:contract:variable-id err))))

  (define file-to-process
    (command-line
     #:program "riposte"
     #:once-each
     [("--version") "Print the version"
                    (opt-version #t)]
     [("--lint") "Check a Riposte file"
                 (opt-lint #t)]
     #:multi
     [("--env") f
                "Specify an environment file to use (can be specified multiple times)"
                (opt-dotenvs (cons f (opt-dotenvs)))]
     #:args ([args #f])
     args))

  (when (opt-version)
    (displayln (format "~a" riposte-version))
    (exit 0))

  (match file-to-process
    [#f
     (do-setup!)
     (define handler (current-eval))
     (define (new-handler x)
       (with-handlers ([exn:fail:contract:variable? complain-about-undefined-var])
         (handler x)))
     (parameterize ([current-namespace (make-base-empty-namespace)])
       (namespace-require '(file "./expander.rkt"))
       (parameterize ([current-eval new-handler])
         (read-eval-print-loop)))]
    [(list filename)
     (set! file-to-process
           filename)]
    [else
     (displayln (format "Cannot make sense of commandline argument: ~a" file-to-process))
     (exit 1)])

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (define-values (dir base is-directory?)
    (split-path file-to-process))

  (when is-directory?
    (displayln (format "Given file is actually a directory: ~a" file-to-process))
    (exit 1))

  (define (lint-complain-and-die! e)
    (displayln (format "~a is a malformed Riposte script.")
               (current-error-port))
    (exit 1))

  (when (opt-lint)
    (with-handlers ([exn:fail? lint-complain-and-die!])
      (file->program file-to-process)
      (displayln (format "~a is a well-formed Riposte script." file-to-process))
      (exit 0)))

  (run! (check-dotenvs (opt-dotenvs)))

  (run! (dotenv-load! (opt-dotenvs)))

  (define cwd
    (cond [(path? dir)
           dir]
          [else
           (current-directory)]))

  (run-program (string->path file-to-process) cwd))
