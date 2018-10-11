#lang racket/base

(require racket/contract
         (only-in (file "./tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "./grammar.rkt")
                  parse))

(define/contract (parse-file path)
  (path-string? . -> . list?)
  (with-input-from-file path
    (lambda ()
      (syntax->datum (parse path (make-tokenizer (current-input-port)))))
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

  (parse-file file-to-process))
