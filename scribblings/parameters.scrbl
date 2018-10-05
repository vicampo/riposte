#lang scribble/manual

@title{Parameters}

There are two parameters (AKA global variables) that steer the way the evaluation of Riposte works.

@itemlist[

@item{base URL}

@item{timeout}

]

@section{Base URI}

Sure, you can write out every URL you want, completely. Or you can save yourself some time, and make your scripts more readable to boot, by using a base URI.

Here's how you can set that:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
%base := "https://whatever.test/v2/"
GET big/boy responds with 2XX
}|

When it comes time to build that @tt{GET} request, we'll use @tt{https://whatever.test/v2/big/boy} as the URI.

@section{Timeout}

Usually we want to wait only a ceratin amount of time before we give up on the server that we're working with. To control that, use @tt{%timeout}, like so:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
%timeout := 5
GET /what/ever responds with 2XX
}|

The value should be a positive integer. (If it isn't, Riposte will die.) We will have

Riposte does not offer a way of disabling a timeout. Just use a big number if you want to give your requests a lot of time.

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
%timeout := 10000000000 # more than 300 years!
GET /what/ever responds with 2XX
}|

The default value for this parameter is 10 seconds.
