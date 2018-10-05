#lang scribble/manual

@title{Installation}

Riposte can be installed with @tt{raco} as follows:

@verbatim{
$ raco pkg install --auto riposte
}

(The @tt{--auto} bit there automatically installs anything that Riposte depends upon, in case you don't have it. You can omit it if you like and be prompted whether you want to install each of the dependencies.)

Once Riposte the package is installed, you ought to have a @tt{riposte} executable in the place where your Racket executables are stored. On my machine (a Mac), @tt{riposte} is residing at @tt{/Users/jessealama/Library/Racket/6.12/bin}. I've added this to my @tt{$PATH} environment variable to make sure that I can just use @tt{riposte} straight away.
