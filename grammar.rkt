#lang brag

riposte-program : program-step*

program-step: assignment
  | command
  | assertion
  | import
  | unset

import: "import" DOUBLE-QUOTED-STRING

unset: "unset" HEADER-IDENTIFIER

assertion : equality | disequality | inequality | predication

equality : expression "=" expression

disequality : expression "!=" expression

inequality : expression ("<" | ">") expression

predication : (expression "is" [ "not" ] json-type)
  | (expression "is" (sequence-adjective | arithmetical-adjective | object-adjective ))
  | (expression "has" "property" expression)
  | (expression "does" "not" "have" "property" expression)
  | (expression "has" "length" expression)
  | (expression "has" "at" [ "least" | "most" ] expression | ( "properties" | "elements" | "characters"))
  | (HEADER-REF "is" ("absent" | "present"))
  | (json-pointer "exists")

json-type : "boolean" | json-number-type | "null" | json-sequence-type | json-object-type

json-sequence-type: [ sequence-adjective ] ( "string" | "array" )

json-number-type: [ arithmetical-adjective ] ( "number" | "integer" | "float" )

json-object-type: [ object-adjective ] "object"

object-adjective: [ "non" ] "empty"

arithmetical-adjective: [ "non" ] ( "positive" | "negative" )

sequence-adjective: [ "non" ] "empty"

assignment : normal-assignment | parameter-assignment | header-assignment

normal-assignment: normal-identifier ":=" expression [ "(" json-type  ")" ]

parameter-assignment: parameter-identifier ":=" expression

header-assignment: head-id ":=" expression

command : ( HTTP-METHOD [ id ] (URI | URI-TEMPLATE) [ "with" "headers" ( normal-identifier | json-array ) ] [ "responds" "with" http-response-code [ "and" satisfies ] ] )
 | HTTP-METHOD [ id ] (URI | URI-TEMPLATE) [ "with" "headers" ( normal-identifier | json-array ) ] [ satisfies ]

satisfies: ("satisfies" | ("does" "not" "satisfy")) "schema" schema-ref

schema-ref: id
  | ("at" (URI | URI-TEMPLATE) )
  | ("in" URI)

expression : json-pointer
  | json-expression
  | id
  | head-id
  | json-number "*" expression
  | expression "+" expression
  | ( "length" "(" expression ")" )

json-pointer: JSON-POINTER [ "relative" "to" normal-identifier ]

json-expression : json-boolean | json-number | json-null | json-array | json-object | json-string

json-boolean : "true" | "false"

json-number : json-float | json-integer

json-float : json-integer+ "." json-integer+

json-integer : DIGIT+

json-null : "null"

json-array : "[" [ json-array-item ("," json-array-item)* ] "]"

json-array-item : (json-expression | id)

json-object : "{" [ json-object-item ("," json-object-item)* ] "}"

json-object-item : json-object-property ":" (json-expression | id)

json-object-property : json-string

json-string: DOUBLE-QUOTED-STRING

http-response-code: (DIGIT DIGIT DIGIT)
  | HTTP-RESPONSE-CODE-PATTERN

id: normal-identifier | env-identifier | parameter-identifier

normal-identifier: IDENTIFIER

env-identifier: ENV-IDENTIFIER
 | (ENV-IDENTIFIER "with" "fallback" DOUBLE-QUOTED-STRING)

head-id: HEADER-IDENTIFIER

parameter-identifier: PARAMETER-IDENTIFIER
