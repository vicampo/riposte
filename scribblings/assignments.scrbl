#lang scribble/manual

@title{Assignments}

With assignments, the idea is that we intend to modify a namespace. There are a few different namespaces in play with Riposte: the variable namespace, HTTP headers, and parameters.

@section{Variable assignments}

A normal definition means that we give a value to a variable.

@section{Header assignments}

Header assignment means that we assign a value to an HTTP header. Until unset, every request will have that header, and that header will have that value. Here's an example:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
^Content-Type := "application/json"
}|

The effect of evaluating this assignment is to indicate that all commands (HTTP requests) executed after this assignment will have a @tt{Content-Type} header, and the value of the header will be @tt{application/json}.

Only strings are allowed as the values for an assignment. The empty string is fine.

@subsection{Request header normalization}

Riposte's approach to HTTP headers is simple: a header can show up only once in a request. Thus, when assigning a value to a request header, any previously existing value for that header will be discarded.

@subsection{Resetting headers}

To ensure that a header does not show up in HTTP requests, use @tt{unset}. Example:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
unset ^Content-Type
}|

Evaluating this means that the next request—and all following ones—will not have a @tt{Content-Type} header.

@include-section["parameters.scrbl"]
