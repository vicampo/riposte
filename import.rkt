#lang racket/base

(provide make-import
         import-step?)

(require racket/class
         racket/contract
         (only-in racket/function
                  const)
         (only-in (file "tokenizer.rkt")
                  make-tokenizer)
         (only-in (file "grammar.rkt")
                  parse)
         (only-in (file "step.rkt")
                  step%)
         (only-in (file "environment.rkt")
                  environment?)
         (only-in (file "parameters.rkt")
                  param-cwd)
         (only-in (file "evaluator.rkt")
                  eval-program))

(define/contract (load-riposte-file path)
  (path? . -> . (or/c false/c list?))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-error "error occurred during expanding an import: ~a" (exn-message e))
                               #f)])
    (define port (open-input-file path))
    (define parse-tree (parse path (make-tokenizer port)))
    (syntax->datum parse-tree)))

(define import%
  (class step%
    (super-new)
    (init-field path)
    (define/override (evaluate env)
      (define p (build-path (param-cwd) path))
      (unless (file-exists? p)
        (error (format "No such file: ~a" (path->string p))))
      (define imported-program (load-riposte-file p))
      (when (eq? imported-program #f)
        (error (format "Malformed Riposte file: ~a"
                       (path->string p))))
      (eval-program imported-program env))
    (define/override (render)
      (format "import \"~a\"" path))))

(define/contract (import-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x import%)))

(define/contract (make-import p)
  (string? . -> . import-step?)
  (new import% [path p]))
