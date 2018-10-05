#lang scribble/manual

@title{Philosophy}

Riposte is intended to be a @emph{language for tests}. As such, it has no concept of branching (“if-then-else”). The idea is that a Riposte script consists of a series of assertions, all of which must succeed. If, during evaluation, a command or assertion fails, evaluation will be aborted. There's no fallback.
