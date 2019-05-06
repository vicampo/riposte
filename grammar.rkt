#lang brag

riposte-program : (/maybe-whitespace (program-step | import) /maybe-whitespace)*

program-step: assignment
  | echo
  | command
  | exec
  | assertion
  | unset

import: /"import" import-filename

import-filename: (LOWERCASE-LETTER | UPPERCASE-LETTER | ZERO | ONE | NON-ZERO-NON-ONE-DIGIT | SLASH | PERIOD | DASH | UNDERSCORE)+

uri-template: ( uri-template-literals | uri-template-expression ) +

uri-template-literals: (@letter | COLON | SLASH | PERIOD | UNDERSCORE | @digit ) +

uri-template-expression: /OPEN-BRACE [ uri-template-operator ] uri-template-variable-list /CLOSE-BRACE

uri-template-operator: "+" | "#" | "." | "/" | ";" | QUESTION-MARK | "&" | "=" | COMMA | "!" | "@" | "|"

uri-template-variable-list: uri-template-varspec ( /COMMA uri-template-varspec ) *

uri-template-varspec: uri-template-varname [ uri-template-variable-modifier ]

@uri-template-varname: @letter ( PERIOD | @letter ) *

uri-template-variable-modifier: uri-template-variable-modifier-prefix | uri-template-variable-modifier-explode

uri-template-variable-modifier-prefix: COLON ( ONE | NON-ZERO-DIGIT ) ( digit ) *

digit: ZERO | ONE | NON-ZERO-NON-ONE-DIGIT

uri-template-variable-modifier-explode: ASTERISK

exec: EXEC URI

echo: "echo" [ json-pointer | normal-identifier | head-id ]

unset: "unset" HEADER-IDENTIFIER

assertion : equality | disequality | inequality | predication

equality : expression /"=" expression

disequality : expression /"!=" expression

inequality : expression ("<" | ">") expression

predication : (expression "is" [ "not" ] json-type)
  | (expression "is" (sequence-adjective | arithmetical-adjective | object-adjective ))
  | (expression "has" "property" expression)
  | (expression "does" "not" "have" "property" expression)
  | (expression "has" "length" expression)
  | (expression "has" "at" [ "least" | "most" ] expression | ( "properties" | "elements" | "characters"))
  | (HEADER-REF "is" ("absent" | "present"))
  | (json-pointer ("exists" | "does" "not" "exist") [ "relative to" (normal-identifier | HEADER-IDENTIFIER) ] [  /"and" "is" [ "non"] "empty" ] )

json-type : "boolean" | json-number-type | "null" | json-sequence-type | json-object-type

json-sequence-type: [ sequence-adjective ] ( "string" | "array" )

json-number-type: [ arithmetical-adjective ] ( "number" | "integer" | "float" )

json-object-type: [ object-adjective ] "object"

object-adjective: [ "non" ] "empty"

arithmetical-adjective: [ "non" ] ( "positive" | "negative" )

sequence-adjective: [ "non" ] "empty"

@assignment : normal-assignment | parameter-assignment | header-assignment

normal-assignment: normal-identifier /":=" expression [ /whitespace "(" /whitespace json-type /whitespace ")" ]

parameter-assignment: PARAMETER-IDENTIFIER /":=" (uri-template | expression)

header-assignment: head-id /":=" expression

command:
    HTTP-METHOD [ /whitespace (id | json-expression) ] /whitespace uri-template [ /whitespace with-headers ] [ emptiness | satisfies | responds-with ]
  | HTTP-METHOD /whitespace ( id | json-expression ) /whitespace uri-template [ with-headers ] [ emptiness | satisfies  | responds-with ]
  | HTTP-METHOD [ id | json-expression ] /whitespace uri-template [ /whitespace with-headers ] (/whitespace (responds-with | satisfies)) [ /whitespace /"and" /whitespace emptiness ]
 | HTTP-METHOD [ /whitespace (id | json-expression) ] /whitespace uri-template [ /whitespace with-headers ] /whitespace responds-with /whitespace /"and" /whitespace (satisfies | emptiness)
  | HTTP-METHOD [ id | json-expression ] uri-template [ with-headers ] responds-with /"and" satisfies /"and" emptiness

with-headers: "with" "headers" ( normal-identifier | json-object )

@satisfies: positive-satisfies | negative-satisfies

positive-satisfies: /"satisfies" /whitespace /"schema" /whitespace schema-ref

negative-satisfies: /"does" /whitespace /"not" /whitespace /"satisfy" /whitespace /"schema" /whitespace schema-ref

emptiness: "is" [ /whitespace "non" ] /whitespace "empty"

responds-with: /"responds with" http-response-code

@schema-ref: id
  | "at" /whitespace uri-template
  | "in" /whitespace uri-template

expression : json-pointer
  | json-expression
  | id
  | head-id
  | expression /whitespace ASTERISK /whitespace expression
  | expression /whitespace PLUS /whitespace expression
  | ( "length" "(" expression ")" )

json-pointer: bare-json-pointer | relative-json-pointer

bare-json-pointer: (SLASH reference-token) +

reference-token: escaped-token | unescaped-token

escaped-token: TILDE (ZERO | ONE)

unescaped-token: (letter | UNDERSCORE | digit) *

@letter: UPPERCASE-LETTER | LOWERCASE-LETTER

relative-json-pointer: bare-json-pointer "relative to" normal-identifier

json-expression : json-boolean | json-number | json-null | json-array | json-object | json-string

json-boolean : "true" | "false"

json-number : json-float | json-integer

json-float : json-integer+ "." json-integer+

json-integer : digit +

json-null : "null"

json-array : /OPEN-BRACKET [ json-array-item (/COMMA json-array-item)* ] /CLOSE-BRACKET

json-array-item : (json-expression | normal-identifier)

json-object : OPEN-BRACE /maybe-whitespace [ json-object-item (/maybe-whitespace COMMA /maybe-whitespace json-object-item)* ] /maybe-whitespace CLOSE-BRACE

json-object-item : json-object-property /maybe-whitespace COLON /maybe-whitespace (json-expression | normal-identifier | env-identifier)

json-object-property : json-string

json-string: DOUBLE-QUOTED-STRING

@http-response-code: (@digit @digit @digit)
  | HTTP-RESPONSE-CODE-PATTERN

id: normal-identifier | env-identifier | parameter-identifier

normal-identifier: IDENTIFIER

env-identifier: ENV-IDENTIFIER
 | (ENV-IDENTIFIER /"with fallback" DOUBLE-QUOTED-STRING)

head-id: HEADER-IDENTIFIER

parameter-identifier: PARAMETER-IDENTIFIER

whitespace: WHITESPACE+

maybe-whitespace: WHITESPACE*