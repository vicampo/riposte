#lang scribble/manual

@title{Where Riposte came from}

Riposte was born out of a need to do more serious testing of HTTP APIs.

I envied the little langauges that I found in developer tools such as @hyperlink["https://github.com/pashky/restclient.el"]{restclient.el} for Emacs and the @hyperlink["https://www.jetbrains.com/help/idea/rest-client-tool-window.html"]{REST Client} in the JetBrains IDEs. (There are surely many more such tools out there.) I wanted to use these kinds of things for automated testing, but couldn't find anything satisfactory. Formats like @hyperlink["https://apiblueprint.org"]{API Blueprint} are a decent start, but I see these more like documentation than an automated test suite. Combining API Blueprint with a tool like @hyperlink["https://dredd.readthedocs.io/en/latest/"]{Dredd} is good, but I found it didn't quite have the features that I was looking for.

So I built Riposte to fill a gap. The requirement was that it needed to have a straightforward language that is more or less instantly recognizable. Riposte is made with Racket, but it was important that the language look like those cool little languages that I alluded to earlier, and @emph{not} like Lisp. Indeeed, Riposte was initially built to be used in an environment where Lisp of any sort was not the language of choice.

In the end, as with many tools in the tech world, I cannot claim that Riposte is totally unique. I was inspired by some good tools, but didn't quite fit the need I had. Perhaps I just needed to RTFM and figure out how to use these tools more intelligently, or perhaps strap some kind of DSL on top of them. Maybe there are tools out there just like Riposte.

But for now, I'm sticking with it. Riposte has proved its usefulness to me and my teammates many times. I hope you find it useful, too.
