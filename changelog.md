# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
(or at least intends to).

## Unreleased

- Support arbitrary HTTP methods, not just GET, PUT, POST, PATCH,
  DELETE, and OPTIONS. (But don't allow CONNECT. For now.)

- Make a build script so that releases can be uploaded easily.

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
