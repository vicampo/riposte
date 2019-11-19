# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
(or at least intends to).

Please submit bugs, feature requests, or other issues to
Riposte's [GitHub Issues
page](https://github.com/vicampo/riposte/issues).

## Unreleased

- Respect `Connection: keep-alive` in responses. (Don't close the
  connection.)
- Respect `Set-Cookie` in responses. Currently, it is entirely ignored
  (!). Simple approach: accept all cookies. More complicated options
  can be imagined:
	  - Permit an option for disallowing all cookies (command line?
        parameter?) `%cookies = false`, `%cookies = true`
	  - Add notation for removing a cookie from the cookie jar (won't
        be included in any future requests). Perhaps: `remove
        ^Cookies[session]`.
	  - Add notation for setting a cookie `^Cookies[foo] := "whatever"`
	  - Add notation for adding a cookie for a single request,
        analogous to the way we can add a header to a single
        request. Something like: `GET /foo with cookies { "session":
        "no" } responds with 2XX`, `GET /foo with additional cookies {
        "session": "whatever" } responds with 2XX` and `GET /foo
        without cookie "session" responds with 2XX`.

## [0.15.0] - 2019-11-19

### Added

- Support for timeouts. The `%timeout` parameter, if set, should be a positive integer. It represents the maximum number of seconds we'll wait when submitting requests. By default, the value is unset, which means that we will wait an unlimited amount of time waiting for a response.
- New "times out" assertion to assert that a request times out. Example: `GET foo/bar times out`. Another example, with a payload: `POST $foo to bar/baz times out`. These commands succeed when the request times out (use the `%timeout` parameter).

## [0.14.0] - 2019-11-15

### Added

- Support for referring to the previous response and earlier responses. Use `!` to refer to the previous response, `!!` to refer to the one before that, and so on (`!!!`, ...).
- Support for disequalities (`$a != $b`).

## [0.13.0]

### Added

- Support for executing external programs. Two forms are
  supported: `exec foo.sh` (just execute the script); `$a :=
  exec foo.sh` (execute the script and parse the standard
  output as JSON, saving the result into a variable. These
  two forms execute the external program with no
  arguments. To add arguments, use the new `with
  arguments`. Example: `exec foo.sh with arguments [
  "--json", $b ]`. The bit after the `with arguments` should
  be a JSON array, written with square brackets, as here, or
  like this: `exec foo.sh with arguments $args`, where
  `$args` refers to a JSON array. The elements of the array
  should all be strings.

## [0.12.4] - 2019-10-13

### Fixed

- A repeat of 0.12.3, essentially!

## [0.12.3] - 2019-10-12

### Fixed

- Tried to fix the same documentation problem that 0.12.2 was trying to fix.

## [0.12.2] - 2019-10-08

### Fixed

- Documentation problems were preventing a build from succeeding (it seems I made a mistake in my symlinks!).

## [0.12.1] - 2019-09-30

### Fixed

- Documentation: Links to example Riposte scripts in
  `scribblings` subdirectory should now work.

## [0.12.0] - 2019-09-30

### Added

- A REPL (read-evaluated-print loop). Just invoke Riposte
  with no arguments, and you're good to go.

## [0.11.1] - 2019-09-21

### Changed

- Added tests to ensure that parsing floating-point numbers works as intended. See the corresponding [GitHub issue](https://github.com/vicampo/riposte/issues/7).

## [0.11.0] - 2019-09-06

### Added

- Checking that the [entire response body is equal to something](https://github.com/vicampo/riposte/issues/6).

### Removed

- Support for "plain" Riposte scripts. Riposte scripts now need to
  start with "#lang riposte", after which there needs to be at least
  one newline.

### Changed

- Syntax for requests with payloads has been
  changed. Previously, they looked like this: `POST $foo
  /bar`. In this version, the keyword `to` needs to be used
  to separate the payload from the URL. Thus, one now should
  write `POST $foo to /bar`.
- It used to be that the the syntax for request payloads required one
  to use an identifier: `POST $foo /bar`. But now you can use
  abritrary JSON expressions (including using variables): `POST { "a":
  $a } /bar` works.

## [0.10.0] - 2018-10-30

### Added

- Support for emptiness for JSON Pointers: `/foo/bar is empty`,
  `/bar/food is non empty`. Can be combined with `exists`: `/foo/bar
  exists and is empty` and `/bar/food exists and is non empty`. The
  concept of "emptiness", as currently defined, makes sense only for
  arrays, lists, and strings. Numbers, booleans, and the null value
  cannot be empty (that is, an error will be raised when evaluating
  emptiness for such values).
- Support for adding objects: `$foo + $bar` will work when both
  variables refer to hashes.

## [0.9.0] - 2018-11-20

### Added

- Better error reporting when a JSON Pointer error occurs.
- "non empty" support: `GET /foo is non empty`. Combines with other
  assertions that are glommed onto commands. Examples: `GET /foo
  responds with 4XX and is empty`; `POST $whatever /a/b satisfies
  schema in file.json and is non empty`.

## [0.8.1] - 2018-11-20

### Added

- Better error reporting when responses do not adhere to a JSON
  Schema.

## [0.8.0] - 2018-11-19

### Added

- When called with --lint, Riposte now parses its argument and reports
  whether it is well-formed. (And does no further work.)

### Fixed

- Executing Riposte with a Riposte file in the current working
  directory now works.

## [0.7.0] - 2018-11-16

### Added

- Support "XXX" as an HTTP response code pattern.
- New command, `exec`, for executing an arbitrary program. Useful for
  doing test setup & teardown.

### Fixed

- Support commands with a payload but without response code check
  (e.g., `POST $foo /whatever`).

## [0.6.0] - 2018-11-14

### Added

- New echo command for echoing the value of a variable or response
  header. There are three variants:
	  + `echo` (no arguments) will echo the body of the previously
        received HTTP response;
	  + `echo $foo` will echo the value of `$foo`
	  + `echo ^Location` will echo the value of the `Location`
        response header.
- `exists` can now be combined with relative JSON Pointers. This now
  works: `/hi exists relative to $foo`.

## [0.5.0]

### Added

- Response code patterns can use a little x as well as a big X.

## [0.4.2]

### Added

- A little program to print our own dependencies (`dependencies.rkt`)

## [0.4.1]

No user visible changes.

### Changed

- Removed some debug output that you shouldn't have seen.

## [0.4.0]

### Added

- Relative JSON Pointers are now supported. By default, when one uses
  a JSON Pointer, it is evaluated with respect to the previously
  received response body.

## [0.3.1]

## [0.3.0]

### Changed

- The arithmetic operator `+` no longer works when one argument is a
  string and the other an integer. Either both are numbers or both are
  strings.

## [0.2.0]

### Added

- The arithmetic operator `+` now takes not only two numbers (as
  before), but also two strings and a string and a non-negative
  integer. Given two strings, `+` concates them. Given a string and a
  non-negative integer *N*, it copies the string *N* times.

- A default `User-Agent` header will be added to all requests. It
  looks like this: `Riposte/0.2.0 (https://riposte.in)`. (See the
  version number there?) Previously, this header was missing (unless
  explicitly added in a Riposte script). As before, you can specify
  your own value for the `User-Agent` header to override this
  default. Currently, it is not possible to revert to the previous
  behavior where `User-Agent` was absent.

- A changelog. (Last but not least.)
