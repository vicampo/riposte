#lang racket/base

(require racket/contract
         syntax/stx
         (only-in (file "./tokenizer.rkt")
                  tokenize)
         (only-in (file "./grammar.rkt")
                  parse))

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

  (require racket/cmdline)

  (define file-to-process
    (command-line
     #:args (filename)
     filename))

  (unless (file-exists? file-to-process)
    (displayln (format "No such file: ~a" file-to-process))
    (exit 1))

  (parse-file (string->path file-to-process)))
