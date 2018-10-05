#lang info

(define collection "riposte")

(define version "0.1.0")

(define deps
  '("base"
    "rackunit-lib"
    "brag"
    "beautiful-racket-lib"
    "web-server-lib"
    "http"
    "argo"
    "dotenv"
    "json-pointer"
    "uri-template"
    "ejs"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "beautiful-racket-lib"))

(define pkg-desc "Riposte is a scripting language for testing JSON-based HTTP APIs.")

(define pkg-authors '("jesse@lisp.sh"))

(define scribblings '(("scribblings/riposte.scrbl" ())))

(define racket-launcher-names '("riposte"))
(define racket-launcher-libraries '("riposte.rkt"))
