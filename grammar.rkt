#lang brag

riposte-program : (program-step | import)*

program-step: assignment
  | echo
  | command
  | exec
  | assertion
  | unset

riposte-repl: program-step

import: /"import" FILENAME

uri-template: ( URI-TEMPLATE-LITERAL | uri-template-expression ) +

uri-template-expression: /"{" [ uri-template-operator ] uri-template-variable-list /"}"

uri-template-operator: "+" | "#" | "." | "/" | ";" | "?" | "&" | "=" | "," | "!" | "@" | "|"

uri-template-variable-list: uri-template-varspec ( /"," uri-template-varspec ) *

uri-template-varspec: IDENTIFIER [ uri-template-variable-modifier ]

uri-template-variable-modifier: uri-template-variable-modifier-prefix | "*"

uri-template-variable-modifier-prefix: ":" NUMBER

digit: ZERO | ONE | NON-ZERO-NON-ONE-DIGIT

exec: EXEC URI

echo: /"echo" [ json-pointer | IDENTIFIER | head-id ]

unset: /"unset" HEADER-IDENTIFIER

assertion : equality | disequality | inequality | predication

equality : expression /"=" expression

disequality : expression /"!=" expression

inequality : expression ("<" | ">") expression

@predication : has-type
  | adjective-applies
  | has-property
  | has-length
  | has-element-count
  | header-presence
  | jp-existence

has-type: expression "is" [ "not" ] json-type

adjective-applies: expression "is" (sequence-adjective | arithmetical-adjective | object-adjective )

has-element-count: expression "has"
  "at" [ "least" | "most" ]
  expression
  ( "properties" | "elements" | "characters" )

has-length: expression "has" "length" expression

has-property: expression ("has" | "does" "not" "have") "property" expression

jp-existence: JSON-POINTER
  ("exists" | "does" "not" "exist")
  [ "relative" "to" (IDENTIFIER | HEADER-IDENTIFIER) ]
  [ "and" "is" [ "non" ] "empty" ]

header-presence: HEADER-REF "is" ("absent" | "present")

json-type : "boolean" | json-number-type | "null" | json-sequence-type | json-object-type

json-sequence-type: [ sequence-adjective ] ( "string" | "array" )

json-number-type: [ arithmetical-adjective ] ( "number" | "integer" | "float" )

json-object-type: [ object-adjective ] "object"

object-adjective: [ "non" ] "empty"

arithmetical-adjective: [ "non" ] ( "positive" | "negative" )

sequence-adjective: [ "non" ] "empty"

@assignment : normal-assignment | parameter-assignment | header-assignment

normal-assignment: IDENTIFIER /":=" expression [ "(" json-type ")" ]

parameter-assignment: PARAMETER /":=" (uri-template | expression)

header-assignment: head-id /":=" expression

command:
    HTTP-METHOD [ (id | json-expression) "to" ] uri-template [ with-headers ] [ emptiness | satisfies | responds-with ]
  | HTTP-METHOD ( id | json-expression ) "to" uri-template [ with-headers ] [ emptiness | satisfies  | responds-with ]
  | HTTP-METHOD [ (id | json-expression) "to" ] uri-template [ with-headers ] (responds-with | satisfies) [ /"and" emptiness ]
 | HTTP-METHOD [ (id | json-expression) "to" ] uri-template [ with-headers ] responds-with /"and" (satisfies | emptiness)
  | HTTP-METHOD [ (id | json-expression) "to" ] uri-template [ with-headers ] responds-with /"and" satisfies /"and" emptiness

with-headers: "with" "headers" ( IDENTIFIER | json-object )

@satisfies: positive-satisfies | negative-satisfies

positive-satisfies: /"satisfies" /"schema" schema-ref

negative-satisfies: /"does" /"not" /"satisfy" /"schema" schema-ref

emptiness: "is" [ "non" ] "empty"

responds-with: /"responds" /"with" HTTP-STATUS-CODE

schema-ref: id
  | "at" uri-template
  | "in" FILENAME

@expression : json-pointer
  | json-expression
  | id
  | head-id
  | expression "*" expression
  | expression "+" expression
  | ( "length" "(" expression ")" )

json-pointer: JSON-POINTER | (JSON-POINTER "relative" "to" IDENTIFIER)

reference-token: escaped-token | unescaped-token

escaped-token: TILDE (ZERO | ONE)

unescaped-token: (letter | UNDERSCORE | digit) *

@letter: UPPERCASE-LETTER | LOWERCASE-LETTER

json-expression : json-boolean | NUMBER | json-null | json-array | json-object | JSON-STRING

json-boolean : "true" | "false"

json-float : json-integer+ "." json-integer+

json-integer : digit +

json-null : "null"

json-array : /"[" [ json-array-item (/"," json-array-item)* ] /"]"

json-array-item : json-expression | IDENTIFIER

json-object : "{" [ json-object-item ("," json-object-item)* ] "}"

json-object-item : json-object-property /":" (json-expression | IDENTIFIER | env-identifier)

json-object-property : JSON-STRING

id: IDENTIFIER | env-identifier | parameter-identifier | head-id

env-identifier: ENV-IDENTIFIER
 | (ENV-IDENTIFIER /"with" /"fallback" json-expression)

@head-id: request-head-id | response-head-id

request-head-id: REQUEST-HEADER-IDENTIFIER

response-head-id: RESPONSE-HEADER-IDENTIFIER

parameter-identifier: PARAMETER-IDENTIFIER
