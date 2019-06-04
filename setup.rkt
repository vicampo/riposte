#lang br

(provide basic-output-port do-setup!)

(require (file "grammar.rkt")
         (file "new-tokenizer.rkt"))

(define basic-output-port
  (make-parameter (open-output-nowhere)))

(define repl-parse (make-rule-parser riposte-repl))

(define (read-one-toplevel-expr origin port)
  (match (read-line port)
    [(? eof-object?)
     eof]
    [(? string? l)
     (define tokenized (tokenize l))
     (displayln (format "tokenized: ~a" tokenized))
     (repl-parse tokenized)]))

(define (do-setup!)
  (basic-output-port (current-output-port))
  (current-read-interaction read-one-toplevel-expr))
