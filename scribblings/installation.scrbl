#lang scribble/manual

@title{Installation}

Riposte is written in @hyperlink["https://racket-lang.org"]{Racket} and depends on an @hyperlink["https://download.racket-lang.org"]{installation} of it. (The previous link takes you to the official Racket download page. You may be able to install Racket using some package manager on your system, such as @hyperlink["https://brew.sh"]{Homebrew} on macOS, @hyperlink["https://wiki.debian.org/DebianPackageManagement"]{apt-get} on Debian-based Linuxes, @hyperlink["https://www.openbsd.org/faq/faq15.html"]{pkg_add} on OpenBSD, and so on and so forth.)

Once you've got Racket installed, use @tt{raco} (the Racket commandline tool) as follows:

@verbatim{
$ raco pkg install --auto riposte
}

(The @tt{--auto} bit there automatically installs anything that Riposte depends upon, in case you don't have it. You can omit it if you like and be prompted whether you want to install each of the dependencies.)

Once Riposte (the Racket package) is installed, you ought to have a @tt{riposte} executable in the place where your Racket executables are stored. On my machine (a Mac), @tt{riposte} is residing at @tt{/Users/jessealama/Library/Racket/7.0/bin}. I've added this to my @tt{$PATH} environment variable to make sure that I can just use @tt{riposte} straight away.
