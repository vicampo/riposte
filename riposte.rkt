#lang racket/base

(require (for-syntax racket/base)
         racket/cmdline
         (only-in racket/file
                  find-files)
         (only-in racket/path
                  path-get-extension)
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
         (only-in (file "./parser.rkt")
                  parse-file
                  is-well-formed?)
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
    (displayln "0.20.0")
    (exit 0))

  (define (is-riposte-file? f)
    (cond [(file-exists? f)
           (match (path-get-extension f)
             [(? bytes? b)
              (bytes=? #".rip" b)]
             [else #f])]
          [else #f]))

  (define (show-error&die err)
    (displayln (string-trim (exn-message err))
               (current-error-port))
    (exit 1))

  (define (riposte-it f)
    (cond [(is-well-formed? f)
           (with-handlers ([exn? show-error&die])
            (dynamic-require f
                             (cond [(opt-lint) (void)]
                                   [else #f])))]
          [else
           (with-handlers ([exn? show-error&die])
             (displayln (format "Malformed Riposte file: ~a" (path->string f))
                        (current-error-port))
             (parse-file f))
           (exit 1)]))

  (match file-to-process
    [(list)
     (run-repl)]
    [(list filename)
     (run! (check-dotenvs (opt-dotenvs)))
     (run! (dotenv-load! (opt-dotenvs)))
     (cond [(directory-exists? filename)
            (define files (find-files is-riposte-file? (string->path filename)))
            (for [(f files)]
              (displayln (format "# ~a" (path->string f)))
              (riposte-it f))]
           [(file-exists? filename)
            (riposte-it (string->path filename))]
           [else
            (displayln (format "No such file: ~a" filename)
                       (current-error-port))
            (exit 1)])]
    [else
     (displayln (format "Cannot make sense of command line argument: ~a" (pretty-print file-to-process))
                (current-error-port))
     (exit 1)]))
