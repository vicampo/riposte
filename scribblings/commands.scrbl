#lang scribble/manual

@title{Commands}

With @emph{commands}, you send out your HTTP requests.

Optionally, you can check that

@itemlist[
  @item{the responses have a certain response code (200, 204, 404, etc.), and}
  @item{that the whole response body satisfies a JSON Schema.}
]

Those two parts are optional; you can check the response code without specifying a schema, and you can specify a schema without saying anything about the response code. You can leave out both, or supply both.

(The Riposte langauge has a concept of an assertion, which is all about checking things. When a command includes a response code check, or a schema check, then commands has the effect of performing an assertion.)

Formally, a command has one of these these structures, depending on whether you want to check the response code and assert that the response satisfies a schema.

@verbatim{
HTTP-METHOD [ PAYLOAD "to" ] URI-TEMPLATE [ "with" "headers" HEADERS ]
}

@verbatim{
HTTP-METHOD [ PAYLOAD "to" ] URI-TEMPLATE [ "with" "headers" HEADERS ] "responds" "with" HTTP-RESPONSE-CODE [ "and" "satisfies" "schema" SCHEMA ]
}

@verbatim{
HTTP-METHOD [ PAYLOAD "to" ] URI-TEMPLATE [ "with" "headers" HEADERS ] "satisfies" "schema" SCHEMA
}

A command need not be on one line.

@section{HTTP methods}

HTTP-METHOD consists of a non-empty sequence of uppercase letters. Typical examples:

@itemlist[

@item{@tt{GET}}

@item{@tt{POST}}

@item{@tt{OPTIONS}}

@item{@tt{PUT}}

@item{@tt{PATCH}}

@item{@tt{DELETE}}

]

However, you can use whatever you like, e.g., @tt{CANCEL}, @tt{BREW}, and so on.

@section{Payload}

PAYLOAD, if present, is supposed to be a variable reference or literal JSON (with variables allowed). Here's an example:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$payload := { "a" : 4, "b": true, "c": [] }
POST $payload to api/flub responds with 2XX
}|

When executing the command, the payload will become the request body.

@section{URI-TEMPLATE}

HTTP requests need to have a URI. (Of course.) The URI could be either fixed (that is, involve no variables)

The URI that will ultimately be built will be glommed onto whatever the base URI is. By default, there is no base URI, so your URI will be used as-is. If a base URI is specified (assign a string value to the global variable @tt{%base} for that purpose), then it will be the prefix for your specified URI. If you wish to go around the base URI, specify an absolute URI, like @tt{https://whatever.test/api/grub} or, before the command, unset @tt{%base}.

If you're not so static, the URI you give here might be built up from URI Templates. Here's a typical example:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$payload := { "a" : 4, "b": true, "c": [] }
$id := 1
POST $payload to api/flub/{id} responds with 2XX
}|

A URI is, at the end of the day, a kind of string. In this example, notice that we've used an integer and plugged it into the URI.

@section{HEADERS}

For a single request, one can specify headers that should be used, in just @emph{this} request, in addition to the ones that are already set. Here's a typical example:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$heads := { "Accept-Language": "jp" }
GET $payload api/flub with headers $heads responds with 2XX
}|

Notice here that we use a JSON object to specify the headers. The headers that are ultimately generated are normalized. (If you have an application that is sensitive to normalization—if it behaves one way when headers are normalized and another if headers are not normalized, I'm afraid Riposte cannot currently build such HTTP requests.)

@section{HTTP-RESPONSE-CODE}

HTTP response codes are supposed to be three-digit integers. There aren't lots of possibilities.

If you don't care about the precise response code, you can use response code patterns. Example: @tt{2XX} means: any 200-class is OK. It could be 200, 201, 204, you name it. You can also say things like @tt{20X} to mean that 200, 201, … 209 would be OK, but 210 wouldn't be.

@section{SCHEMA}

There are two forms that SCHEMA can take:

@itemlist[

@item{a variable reference, or}

@item{an unquoted string.}

]

The two forms are for referring directly to a JSON Schema as a JSON value or in an external file. Here are examples of the two kinds of forms:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$schema := {
  "type": "object",
  "requiredProperties": [ "age", "weight" ]
}
GET $payload api/flub satisfies schema $schema
}|

Example of using a schema specified in a file:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
GET $payload api/flub satisfies schema in schema.json
}|
