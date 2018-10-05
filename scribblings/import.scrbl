#lang scribble/manual

@title{Imports}

Imports are a way of putting some Riposte code into one file and using it in another. It gives a way to pass variables from one script to another. They allow you to carry out some requests, gathering a bit of state, and then using that state in another file.

Here's an example of this. Let's call this file @tt{headers.rip}:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
^Content-Type := "application/json"
^Cache-Control := "no-cache"
}|

Consider setting up a base URI in a separate file, @tt{parameters.rip}:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
%base := "https://cool.test/api/v2/"
%timeout := 30
}|

Then, in another Riposte script, @tt{login.rip}, we can import these two to set up a kind of basis:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
import "parameters.rip"
import "headers.rip"

$payload := {
  "username": @USERNAME,
  "password": @PASSWORD
}

POST $payload login responds with 200

$key := /apikey
^APIKey := $key
}|

The @tt{POST} command here gets executed here with the parameters and headers set up in the other files. Upon executing @tt{login.rip}, a header, @tt{APIKey}, gets set. @tt{login.rip} also ``exports'' a variable, @tt{$key}. If you were to keep going an import @tt{login.rip}, you'd have that variable, and the header assignment would also be in place:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
import "login.rip"

# You can refer to $key

$load := {
  "key": $key, # comes from login.rip
  "zoom": "boom"
}

# APIKey request header will be present here:
PATCH $load dump/truck responds with 204
}|
