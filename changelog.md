# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
(or at least intends to).

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
