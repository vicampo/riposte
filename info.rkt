#lang info

(define collection "riposte")

(define version "0.21.3")

(define deps
  '("br-parser-tools-lib"
    "brag-lib"
    "net-cookies-lib"
    "web-server-lib"
    "base"
    "racket-doc"
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

(define compile-omit-paths '("examples/"))
