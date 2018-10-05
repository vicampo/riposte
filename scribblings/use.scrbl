#lang scribble/manual

@title{Usage}

There are two ways of executing a Riposte script: using @tt{riposte} or directly executing them.

@section{Using the interpreter}

Do this:

@verbatim[]|{
$ riposte path/to/script.rip
}|

@section{Direct execution}

Use a shebang. Start your Riposte script like this:

@verbatim[]|{
#!/usr/local/bin/riposte
#
GET whatever responds with 2XX
}|

Make sure your Riposte script is executable. Then—assuming that Riposte really is at @tt{/usr/bin/riposte}, as indicated in the script—you ought to be able to just to do

@verbatim[]|{
$ ./let-er.rip
}|
