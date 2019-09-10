#lang brag

riposte-program : (program-step | import)*

@program-step: assignment
  | echo
  | command
  | exec
  | assertion
  | unset

import: /"import" FILENAME

uri-template: ( URI-TEMPLATE-LITERAL | uri-template-expression ) +

uri-template-expression: /"{" [ uri-template-operator ] uri-template-variable-list /"}"

uri-template-operator: "+" | "#" | "." | "/" | ";" | "?" | "&" | "=" | "," | "!" | "@" | "|"

uri-template-variable-list: uri-template-varspec ( /"," uri-template-varspec ) *

uri-template-varspec: IDENTIFIER [ uri-template-variable-modifier ]

uri-template-variable-modifier: uri-template-variable-modifier-prefix | "*"

uri-template-variable-modifier-prefix: ":" NUMBER

digit: ZERO | ONE | NON-ZERO-NON-ONE-DIGIT

exec: /"exec" FILENAME
  | /"exec" FILENAME /"with" /"arguments" normal-identifier
  | /"exec" FILENAME /"with" /"arguments" "[" [ exec-arg-item (/"," exec-arg-item)* ] "]"

exec-arg-item: normal-identifier | JSON-STRING

echo: /"echo" [ json-pointer | normal-identifier | head-id ]

unset: /"unset" REQUEST-HEADER-IDENTIFIER

@assertion : equality | disequality | inequality | predication

equality : expression /"=" expression

disequality : expression /"!=" expression

inequality : expression ("<" | ">" | "<=" | ">=") expression

@predication : has-type
  | has-property
  | has-length
  | has-element-count
  | header-presence
  | jp-existence
  | sequence-predicate

has-type: expression "is" [ "non" ] adjective
  | expression "is" [ "not" ] ( "a" | "an" ) [ [ "non" ] adjective ] json-type

@adjective: sequence-adjective
  | arithmetical-adjective
  | object-adjective
  | "null"

sequence-predicate: expression ("starts" | "ends" ) "with" expression

has-element-count: expression "has"
  [ "at" ( "least" | "most" ) ]
  expression
  ( "properties" | "elements" | "characters" )

has-length: expression "has" "length" expression

has-property: expression ("has" | "does" "not" "have") "property" expression

jp-existence: JSON-POINTER
  ("exists" | ("does" "not" "exist"))
  [ "relative" "to" (normal-identifier | HEADER-IDENTIFIER) ]
  [ ("and" "is" jp-value-adjective) |  ("and" "is" ("a" | "an") jp-value-adjective json-type) ]

@jp-value-adjective: [ "non" ] ("empty" | "negative" | "positive")

header-presence: RESPONSE-HEADER-IDENTIFIER /"is" ("absent" | "present")

@json-type : "null"
  | "boolean"
  | "string"
  | "number"
  | "integer"
  | "object"
  | "array"

object-adjective: [ "non" ] "empty"

@arithmetical-adjective: [ "non" ] ( "positive" | "negative" | "even" | "odd" )

@sequence-adjective: [ "non" ] "empty"

@assignment : normal-assignment | parameter-assignment | header-assignment

normal-assignment: IDENTIFIER /":=" expression [ "(" [ adjective ] json-type ")" ]

parameter-assignment: PARAMETER /":=" (uri-template | expression)

header-assignment: REQUEST-HEADER-IDENTIFIER /":=" expression

command:
    HTTP-METHOD [ payload /"to" ] uri-template [ with-headers ] [ emptiness | satisfies | responds-with ]
  | HTTP-METHOD [ payload /"to" ] uri-template [ with-headers ] (responds-with | satisfies) [ /"and" emptiness ]
 | HTTP-METHOD [ payload /"to" ] uri-template [ with-headers ] responds-with /"and" (satisfies | emptiness)
| HTTP-METHOD [ payload /"to" ] uri-template [ with-headers ] responds-with /"and" satisfies /"and" emptiness

@payload: id
  | json-expression

with-headers: /"with" /"headers" ( normal-identifier | json-object )

@satisfies: positive-satisfies | negative-satisfies

positive-satisfies: /"satisfies" /"schema" schema-ref

negative-satisfies: /"does" /"not" /"satisfy" /"schema" schema-ref

emptiness: "is" [ "non" ] "empty"

responds-with: /"responds" /"with" HTTP-STATUS-CODE

schema-ref: id
  | json-expression
  | "at" uri-template
  | "in" FILENAME

expression: json-pointer
  | json-expression
  | id
  | head-id
  | expression ("*" | "+" | "-") expression

json-pointer: JSON-POINTER | (JSON-POINTER "relative" "to" normal-identifier)

reference-token: escaped-token | unescaped-token

escaped-token: TILDE (ZERO | ONE)

unescaped-token: (letter | UNDERSCORE | digit) *

@letter: UPPERCASE-LETTER | LOWERCASE-LETTER

@json-expression: json-boolean | NUMBER | json-null | json-array | json-object | JSON-STRING

json-boolean : "true" | "false"

json-float : json-integer+ "." json-integer+

json-integer : digit +

json-null : "null"

json-array : /"[" [ json-array-item (/"," json-array-item)* ] /"]"

json-array-item : json-expression | normal-identifier

json-object: /"{" [ json-object-item (/"," json-object-item)* ] /"}"

json-object-item: JSON-STRING /":" (json-expression | normal-identifier | env-identifier)

@id: normal-identifier | env-identifier | parameter-identifier | head-id

normal-identifier: IDENTIFIER

env-identifier: ENV-IDENTIFIER
 | (ENV-IDENTIFIER /"with" /"fallback" json-expression)

@head-id: request-head-id | response-head-id

request-head-id: REQUEST-HEADER-IDENTIFIER

response-head-id: RESPONSE-HEADER-IDENTIFIER

parameter-identifier: PARAMETER-IDENTIFIER

riposte-repl: normal-identifier | command | assignment
