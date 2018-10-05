#lang scribble/manual

@title{Limitations}

All sorts of HTTP scenarios cannot be modeled at all with Riposte. Riposte is essnetially a scriptable headless browser, but you can't do @emph{everything} at all.

@itemlist[

@item{As mentioned earlier, Riposte works with JSON-only (with one exception: responses that contain no body at all). A non-empty response that is malformed JSON will cause Riposte to bail out.}

@item{No support for redirections. The HTTP client built in to Riposte can, but deliberately does not, follow redirections. The intention is that a resquest is supposed to generate an ``immediate'' response. It wouldn't be }

@item{No support for the @tt{CONNECT} HTTP method.}


]
