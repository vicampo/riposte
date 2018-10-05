#lang scribble/manual

@require[@for-label[racket/base riposte]]

@title[#:style "toc"]{Riposte—Scripting Language for JSON-based HTTP APIs}
@author[(author+email "Jesse Alama" "jesse@lisp.sh")]

Riposte is a scripting language for evaluating JSON-bearing HTTP responses. The intended use case is a JSON-based HTTP API. It comes with a commandline tool, @tt{riposte}, which executes Riposte scripts. Using Riposte, one writes a sequence of commands—which amount to HTTP requests—and assertions, which require that the response meets certain conditions.

Riposte exists to help improve testing for HTTP APIs. I've found that it's not enough to test a handful of URIs to make sure they give a sensible response. For more complicated scenarios, it's important to test a single endpoint many times, with different payloads, expecting different responses.

In other words, Riposte was made to help you get more serious about testing your APIs.

@emph{Clarification} The word ``Riposte'' might refer to the scriping language, to the commandline program @tt{riposte}, or to the intended semantics of the Riposte language, which is implemented in the @tt{riposte} program. So when we say things like ``Riposte allows this'', ``Riposte bails out'', or ``call Riposte this way'', we have one of these possible meanings in mind. The context of the statement should make it clear how to disambiguate; if it is not clear what is meant, please file a bug ticket that asks for clarification.

@include-section["installation.scrbl"]

@include-section["briefly.scrbl"]

@include-section["use.scrbl"]

@include-section["language.scrbl"]

@include-section["limitations.scrbl"]

@include-section["misc.scrbl"]

@include-section["wishlist.scrbl"]
