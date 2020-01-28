#lang info

(define collection "riposte")

(define version "0.19.1")

(define deps
  '("br-parser-tools-lib"
    "brag-lib"
    "net-cookies-lib"
    "web-server-lib"
    "base"
    "brag-lib"
    "br-parser-tools-lib"
    "beautiful-racket-lib"
    "http"
    "net-cookies-lib"
    "argo"
    "dotenv"
    "json-pointer"
    "misc1"))

(define build-deps
  '("scribble-lib"
    "rackunit-lib"
    "web-server-lib"
    "net-cookies-lib"
    "beautiful-racket-lib"))

(define pkg-desc "Scripting language for testing JSON-based HTTP APIs.")

(define pkg-authors '("jesse@lisp.sh"))

(define scribblings '(("scribblings/riposte.scrbl" ())))

(define racket-launcher-names '("riposte"))
(define racket-launcher-libraries '("riposte.rkt"))

(define compile-omit-paths '("examples/"))
