#lang scribble/manual

@title{Brief introduction}

Let's take a look at some little Riposte scripts.

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
# set a header, to be added to all requests going forward
# the # character is the to-end-of-line comment character

^Content-Type := "application/json"
# set a base URL; we will merge all URIs going forward
# with this
%base := https://api.example.com:8441/v1/

$uuid := @UUID # @ is how you access environment variables
}|

Normal variables (here, @tt{$uuid}) have the @tt{$} sigil in front of them. There are a handful of global variables which use @tt{%} as their sigil (here, @tt{%base}). Headers form their own kind of “namespace“ for variables; their sigil is @tt{^} (here, @tt{^Content-Type}). In all cases, @tt{:=} is the way you assign a value to a variable.

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
# -----------------------
# Start sending requests!
# -----------------------

GET cart/{uuid} responds with 2XX
}|

Riposte uses @hyperlink["https://tools.ietf.org/html/rfc6570"]{URI Template} as its language for URIs. URI Template extends URIs with a kind of template syntax. Values get plugged in between open and close curly braces. Here, we plug in the value of the @tt{uuid} variable, whose value came from the environment. The @tt{2XX} means: we expect a 200-class response; the precise response code doesn't matter. It could be 200, 201, 204...

Riposte supports @tt{GET}, @tt{HEAD}, @tt{POST}, @tt{PATCH} and @tt{DELETE} as HTTP request methods.

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
# ----------------------------------------------------------------------
# Now add something to the cart:
# ----------------------------------------------------------------------

$productId := 41966
$qty := 5
$campaignId := 1

$payload := {
  "product_id": $productId,   # we extend the JSON syntax here
  "campaign_id": $campaignId, # in that you can use Riposte variables
  "qty": $qty                 # and you can add comments to JSON, too
}

POST $payload cart/{uuid}/items responds with 200

$itemId := /items/0/cart_item_id # extract the item ID
}|

Here, we define some data and build a JSON object with three properties. We then submit that to (what works out to) @tt{https://api.example.com/cart/28f896e6-98df-497b-b95f-c34d39c2a368/items}. (I just picked a random UUID.) Notice that we expect a concrete response code here. The response had better be 200. Anything else would be an error.

We also extract data from the response. Riposte builds on JSON Pointer, an IETF standard notation for referring to parts of JSON documents.

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
GET cart responds with 2XX

/tax_total exists
}|

After making a request, you can make assertions about it. Here, with @tt{exists}, we're asserting that (1) the response is a JSON object, and (2) it has a property, @tt{"tax_total"}. We don't care what that value is. (Riposte comes with a ton of assertions; @tt{exists} is just one.)

Here's another Riposte script that logs in to our iOS API:

@codeblock[#:keep-lang-line? #f]|{
#lang riposte
$loginPayload := {
  "email": @EMAIL,
  "password": @PASSWORD
}

POST $loginPayload auth/login responds with 2XX

# extract a value from the response body
# and use it as the value of an
# HTTP header, going forward:
^Apikey := /key
}|

Welcome to Riposte. En garde!
