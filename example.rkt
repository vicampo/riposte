#lang racket

#|

#lang riposte
GET https://api.vicampo.test/v1/auth/login responds with 405
POST $x https://api.vicampo.test/v1/auth/login responds with 405


|#

#|

(command 'GET #f "https://api.vicampo.test/v1/auth/login")
(check-response-code 405)
(command 'POST 'x "https://api.vicampo.test/v1/auth/login")
(check-response-code 405)

|#
