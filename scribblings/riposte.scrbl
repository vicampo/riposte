#lang scribble/manual

@title[#:style 'toc]{Riposte—Scripting Language for JSON-based HTTP APIs}
@author[(author+email "Jesse Alama" "jesse@lisp.sh")]

@defmodulelang[riposte]

Riposte is a scripting language for evaluating JSON-bearing HTTP responses. The intended use case is a JSON-based HTTP API. It comes with a commandline tool, @tt{riposte}, which executes Riposte scripts. Using Riposte, one writes a sequence of commands—which amount to HTTP requests—and assertions, which require that the response meets certain conditions.

Riposte is intended to be a language for system (or integration) tests. As such, it is not intended to be a general purpose programmign langauge. There is no notion of branching (“if-then-else”) in a single script. The idea is that a Riposte script consists of a series of assertions, all of which must succeed. If, during evaluation, a command or assertion fails, evaluation will be aborted. There's no fallback.

In other words, Riposte was made to help you get more serious about testing your APIs. It helped me up my testing game, and I hope it can do the same for you.

@include-section["briefly.scrbl"]

@include-section["installation.scrbl"]

@include-section["origins.scrbl"]

@include-section["use.scrbl"]

@include-section["language.scrbl"]

@include-section["limitations.scrbl"]

@include-section["misc.scrbl"]

@include-section["wishlist.scrbl"]
