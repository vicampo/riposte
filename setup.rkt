#lang br

(provide basic-output-port do-setup!)

(require "grammar.rkt" "new-tokenizer.rkt")

(define basic-output-port
  (make-parameter (open-output-nowhere)))

(define repl-parse (make-rule-parser riposte-repl))

(define (read-one-toplevel-expr origin port)
  (define (read-until-parsed lines)
    (match (read-line port)
      [(? eof-object?)
       eof]
      [(? string? l)
       (define all-lines (append lines (list l)))
       (define all-lines+newlines (map (lambda (s)
                                         (~a s #\newline))
                                       all-lines))
       (with-handlers ([exn:fail? (lambda (e)
                                    (read-until-parsed (append lines (list l))))])
         (define tokenized (tokenize (apply string-append all-lines+newlines)))
         (log-error "tokenized: ~a" tokenized)
         (repl-parse tokenized))]))
  (read-until-parsed (list)))

(define (do-setup!)
  (basic-output-port (current-output-port))
  (current-read-interaction read-one-toplevel-expr))
