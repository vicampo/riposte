#lang racket/base

(provide is-well-formed?
         parse-file)

(require racket/contract
         syntax/stx
         (only-in (file "./tokenizer.rkt")
                  tokenize)
         (only-in (file "./grammar.rkt")
                  parse))

(define (is-well-formed? path)
  (define-values (dir base is-directory?)
    (split-path path))
  (define (parse-it)
    (syntax->datum
     (parse path (tokenize (current-input-port)))))
  (cond [(file-exists? path)
         (with-handlers ([exn:fail? (lambda (e) #f)])
           (begin0
               (with-input-from-file path parse-it #:mode 'text)
             #t))]
        [else #f]))

(define/contract (parse-file path)
  (path? . -> . list?)
  (define-values (dir base is-directory?)
    (split-path path))
  (when is-directory?
    (error (format "Cannot parse directories: ~a" (path->string path))))
  (with-input-from-file path
    (lambda ()
      (syntax->datum
       (parse path (tokenize (current-input-port)))))
    #:mode 'text))

(module+ main

  (require racket/cmdline
           racket/pretty)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process)
               (current-error-port))
    (exit 1))

  (pretty-print (parse-file (string->path file-to-process))))
