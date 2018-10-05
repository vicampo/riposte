Riposte—Scripting Language for JSON-based HTTP APIs
==========

Riposte is a scripting language for evaluating JSON-bearing HTTP responses. The intended use case is a JSON-based HTTP API. It comes with a commandline tool, `riposte`, which executes Riposte scripts. Using Riposte, one writes a sequence of commands—which amount to HTTP requests—and assertions, which require that the response meets certain conditions.

Examples:

    # set a header, to be added to all requests going forward
	# the # character is the to-end-of-line comment character

    ^Content-Type := "application/json"

	# set a base URL; we will merge all URIs going forward
	# with this
    %base := "https://api.mydomain.test:8441/v1/"

    $uuid := @UUID # @ is how you access environment variables

Normal variables (here, `$uuid`) have the `$` sigil in front of them. There are a handful of global variables which use `%` as their sigil (here, `%base`). Headers form their own kind of “namespace“ for variables; their sigil is `^` (here, `^Content-Type`). In all cases, `:=` is the way you assign a value to a variable.

    # ----------------------------------------------------------------------
    # Start sending requests
    # ----------------------------------------------------------------------

    GET checkout/cart/{uuid} responds with 2XX

Riposte builds on URI Template to express URIs. Here, we plug in the value of the `uuid` variable, which came from the environment. The `2XX` means: we expect a 200-class response. The precise response code doesn't matter.

Riposte supports `GET`, `HEAD`, `POST`, `PATCH` and `DELETE` as HTTP request methods.

    # ----------------------------------------------------------------------
    # Now add something to the cart:
    # ----------------------------------------------------------------------

    $productId := 41966
    $qty := 5

    $payload := {
      "product_id": $productId, # we extend the JSON syntax here
      "qty": $qty # and you can add comments to JSON, too
    }

    POST $payload checkout/cart/{uuid}/items responds with 200

    $itemId := /vendors/0/items/0/cart_item_id # extract the item ID

Here, we define some data and build a JSON object with three properties. We then submit that to (what works out to) `https://api.mydomain.test/v1/checkout/cart/28f896e6-98df-497b-b95f-c34d39c2a368/items`. (I just picked a random UUID.) Notice that we expect a concrete response code here. The response had better be 200. Anything else would be an error.

We also extract data from the response. Riposte builds on JSON Pointer, which is an IETF standard notation for referring to parts of JSON documents.

    GET checkout/cart responds with 2XX

    /tax_total exists

After making a request, you can make assertions about it. Here, with `exists`, we're asserting that (1) the response is a JSON object, and (2) it has a property, `"tax_total"`. We don't care what that value is. (Riposte comes with a ton of assertions; `exists` is just one.)

Here's another Riposte script that logs in to an API by `POST`ing credentials (a JSON array) to a certain endpoint, which is supposed to give us back a key that we will attach to later requests:

    $loginPayload := {
        "email": @EMAIL,
        "password": @PASSWORD
    }

    POST $loginPayload auth/login responds with 2XX

    ^Apikey := /key # extract a value from the response body
	# and use it as the value of an HTTP header, going forward

Welcome to Riposte.

## Approach to testing ##

Riposte is intended to be a **language for tests**. As such, it has no concept of branching (“if-then-else”). The idea is that a Riposte script consists of a series of assertions, all of which must succeed. If, during evaluation, a command or assertion fails, evaluation will be aborted. There's no fallback.

**Clarification** The word “Riposte” might refer to the language, to the commandline program `riposte`, or to the intended semantics of the Riposte language, which is implemented in the `riposte` program. So when we say things like "Riposte allows this", "Riposte bails out", or "call Riposte this way", we have one of these possible meanings in mind. The context of the statement should make it clear how to disambiguate; if it is not clear what is meant, please file a bug ticket that asks for clarification.)

# JSON-only #

Riposte is JSON-only. It works with JSON values and assumes that the responses it receives (that is, the response bodies) are JSON.

Some HTTP responses (that is, their bodies) are empty. There's even an HTTP response code for this case (`204`). Riposte can handle empty response bodies.  If, however, a non-empty response is returned, Riposte attempts to parse the body as JSON. If if can't, Riposte will die.

This means that Riposte (in its current incarnation) can't work with HTML, arbitrary plain text, images, and so on. If you request a URI and get a response that contains, say, HTML, Riposte will bail out (unless, by chance, the response turns out to be well-formed JSON!).

# File extension #

Use `.rip`.

(Valid Riposte files are plain UTF-8 text, and no check is made that the file has the extension `.rip`. You could just as well use `.txt`, `.ribfest`, `.nsa`…)

# Commandline usage #

There are two ways of executing a Riposte script: using `riposte` or directly executing them.

## Using the interpreter ##

Do this:

    $ riposte path/to/script.rip

## Direct execution ##

Use a shebang. Start your Riposte script like this:

     #!/usr/local/bin/riposte
	 #
	 GET whatever responds with 2XX

Make sure your Riposte script is executable. Then—assuming that Riposte really is at `/usr/local/bin/riposte`—the following should suffice:

    $ ./let-er.rip
