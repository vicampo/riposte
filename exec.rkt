#lang racket/base

(provide make-exec-step
         exec-step?)

(require racket/contract
         racket/class
         racket/system
         racket/port
         racket/string
         racket/format
         (only-in (file "./step.rkt")
                  step%))

(define/contract (render-commented-out str)
  (string? . -> . void)
  (for ([line (string-split str (~a #\newline))])
    (displayln (format "# ~a" line))))

(define/contract exec%
  class?
  (class step%
    (super-new)
    (init-field path)
    (define/override (evaluate env)
      (define command (path->string path))
      (define successful? #f)
      (define (do-it p)
        (define new-input (make-limited-input-port (current-input-port)
                                                   0
                                                   #f))
        (parameterize [(current-input-port new-input)
                       (current-output-port p)
                       (current-error-port p)]
          (when (system command)
            (set! successful? #t))))
      (displayln (send this render))
      (define output (call-with-output-string do-it))
      (render-commented-out output)
      (unless successful?
        (error (format "Execution of ~a failed." command)))
      env)
    (define/override (render)
      (format "exec ~a" (path->string path)))))

(define/contract (exec-step? x)
  (any/c . -> . boolean?)
  (and (object? x)
       (is-a? x exec%)))

(define/contract (make-exec-step e)
  (path? . -> . exec-step?)
  (new exec%
       [path e]))
