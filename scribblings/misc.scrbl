#lang scribble/manual

@title{Miscellanea}

Here's where you can find some information for which I couldn't find a better place.

@section{File extension}

I suggest using @tt{.rip} for your Riposte scripts. But, in truth, it doesn't matter. Riposte scripts are supposed to be plain text, and there's no check that a script has any particular file extension.

@section{JSON-only}

Riposte works with JSON values and assumes that the responses it receives (more precisely, the response bodies) are JSON.

There's one exception to this. Some HTTP responses (again, their bodies) are empty. There's even an HTTP response code for this case (@tt{204}). Riposte can handle such responses, even though the empty string is not valid JSON.

If, however, a non-empty response is returned, Riposte attempts to parse the body as JSON. If if can't, Riposte will die.

This means that Riposte, currently, can't work with HTML, arbitrary plain text, images, and so on. If you request a URI and get a response that contains, say, HTML, Riposte will bail out (unless, by chance, the response turns out to be well-formed JSON!).

@section{Draft 07 Schema only}

Currently, schema validation with Riposte is done according to JSON Schema draft 07. There is, at the moment, no support for any other draft version of JSON Schema. Sorry.
