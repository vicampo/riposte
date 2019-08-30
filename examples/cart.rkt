#lang racket/base

(require racket/match
         web-server/http/request-structs
         web-server/dispatch
         json
         (file "common.rkt"))

(define (view-cart req cart-id)
  (response/jsexpr (hasheq 'grand_total 34.58)
                   #:code 200))

(define (add-item-to-cart req cart-id)
  (match (request-post-data/raw req)
    [#f
     (response/empty #:code 400)]
    [(? bytes? bs)
     (with-handlers ([exn:fail? (lambda (err)
                                  (response/empty #:code 400))])
       (match (bytes->jsexpr bs)
         [(hash-table ('product_id (? exact-positive-integer? product-id))
                      ('campaign_id (? exact-positive-integer? campaign-id))
                      ('qty (? exact-positive-integer? qty)))
          (define item (hasheq 'product_id product-id
                               'campaign_id campaign-id
                               'qty qty
                               'cart_item_id 42))
          (define price (* qty (+ product-id campaign-id (random 1 25))))
          (response/jsexpr (hasheq 'grand_total price
                                   'items (list item))
                           #:code 200)]
         [else
          (response/empty #:code 400)]))]))

(define-values (start url-generator)
  (dispatch-rules
   [("cart" (string-arg)) #:method "get" view-cart]
   [("cart" (string-arg) "items") #:method "post" add-item-to-cart]))

(module+ main
  (run start))
