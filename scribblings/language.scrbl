#lang scribble/manual

@title{Language}

The Riposte language consists of three basic elements:

@itemlist[
  @item{@emph{assignments},}

  @item{@emph{imports},}

  @item{@emph{commands}, and}

  @item{@emph{assertions}}

]

Here's a simple Riposte script contains all three ingredients:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
^Content-Type := "application/json"
$payload := { "a": 5, "b": [] }
POST $payload https://whatever.test responds with 201
/foo is positive integer
}|

Lines 1 and 2 are assignments; the first assigns a value to a header, and the second assigns a value to a variable. Line 3 is a command: here, we send a @tt{POST} request, with a payload—a JSON object specified in line 2. We expect that the response we receive has an exit code of 201. Line 4 is an assertion. We're saying there that the response body had better be well-formed JSON, that that JSON had better be a JSON object, and that that object had better have the property @tt{foo}, and the value of the @tt{foo} property of that object is a positive integer.

The following sections give a more thorough discussion of the different kinds of top-level syntatic ingredients.

@include-section["import.scrbl"]

@include-section["assignments.scrbl"]

@include-section["commands.scrbl"]

@include-section["assertions.scrbl"]
