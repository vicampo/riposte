#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         racket/pretty
         racket/format
         racket/string
         dotenv
         (file "util.rkt")
         (only-in (file "./version.rkt")
                  riposte-version)
         racket/contract)

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

(module+ main

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
    (displayln (format "No such file: ~a" file-to-process)
               (current-error-port))
    (exit 1))

  (run! (check-dotenvs (opt-dotenvs)))

  (run! (dotenv-load! (opt-dotenvs)))

  (with-handlers ([exn? (lambda (err)
                          (displayln (string-trim (exn-message err))
                                     (current-error-port))
                          (exit 1))])
    (dynamic-require (string->path file-to-process) #f)))
