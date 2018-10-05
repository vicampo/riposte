#lang scribble/manual

@title{Assertions}

Assertions are checks that succeed or fail. When executing an assertion, Riposte will see whether what's being asserted is true. If it's true, it moves on to the next thing (command, assertion, etc.). If the check fails, Riposte bails out.

There are a few different kinds of assertions:

@itemlist[

@item{equations}
@item{disequations}
@item{inequalities}
@item{type checks}

]

@section{Equations & their discontents (disequations, inequalities)}

Write an equation by writing two expressions separated by @tt{=}.

Write a disequation by writing @tt{!=}

@section{Type checks}

You can use @tt{is} and the JSON type keywords to assert that a value has a certain type. The types are:

@itemlist[

@item[null]

@item{number}

@item{integer}

@item{boolean}

@item{string}

@item{array}

@item{object}

]

You can use these words as-is. Thus:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$foo is integer
}|

is an assertion that succeeds provided that the value held by @tt{$foo} is indeed an integer.

@section{Negation}

You can assert that a value does not have a certain type by using @tt{not}. Thus:

@codeblock[#:keep-lang-line? #f #:line-numbers #f]|{
#lang riposte
$bar is not array
}|

works provided that @tt{$bar} is not an array.

@section{Adjectives}

One can tweak checks with adjectives. The set of adjectives available depends on the type.

Use @tt{non} to say that a value does not meet a certain adjective.

@subsection{Numeric adjectives}

@itemlist[

@item{@tt{positive} / @tt{non positive}}

@item{@tt{negative} / @@tt{non negative}}

]

@subsection{String adjectives}

@itemlist[

@item{@tt{empty} / @tt{non empty}}

]

@subsection{Array adjectives}

@itemlist[

@item{@tt{empty} / @tt{non empty}}

]

@subsection{Object adjectives}

@itemlist[

@item{@tt{empty} / @tt{non empty}}

]
