#lang racket/base

(require net/url)
(require net/http-client)
(require json)

(provide light-set-bri-hue! ; id bri hue username -> pure-port
         light-set-bri-ct!  ; id bri ct username -> pure-port
         light-on?          ; id username -> Bool
         light-get          ; id username -> jsexpr
         MAX-BRI 
         RED GREEN BLUE)

;; Limits

(define MAX-HUE 65535)
(define MAX-BRI 254)
(define MAX-SAT 254)

;; Colours

;; Range of "colour temperature"
;; The hue colour can be set as a temperature in "mireds"
;; https://en.wikipedia.org/wiki/Mired
(define MIN-CT 153) ; 6500 K
(define MAX-CT 500) ; 2000 K

;; Range of "hue"
(define RED       0)
(define GREEN 25500)
(define BLUE  46920)

;; Net

(define BASE-URL
  (make-url "http"                                    ; scheme
            #f                                        ; user
            "philips-hue.home"                        ; host
            #f                                        ; port
            #t                                        ; path-absolute?
            null                                      ; path
            null                                      ; query
            #f))                                      ; fragment

  
;; Is the light on?
(define (light-on? id username)
  (hash-ref
   (hash-ref (light-get id username) 'state #f)
   'on #f))

;; Get JSON of light status
(define (light-get id username)
  (define in (get-pure-port (light-get-url id username)))
  (read-json in))

(define (light-get-url id username)
  (make-hue-url (list "api" username "lights" (number->string id)))) 


;; Set light with id to given brightness and colour, given username
;; brightness, and hue using Hue scale
(define (light-set-bri-hue! id brightness hue username)
  (put-pure-port
   (light-put-url id username)
   (jsexpr->bytes
    (hasheq 'bri brightness
            'hue hue
            'sat MAX-SAT))))

;; Set light to given brightness and colour temp
(define (light-set-bri-ct! id brightness ct username)
  (put-pure-port
   (light-put-url id username)
   (jsexpr->bytes
    (hasheq 'bri brightness
            'ct  ct))))

(define (light-put-url id username)
  (make-hue-url (list "api" username "lights" (number->string id) "state"))) 
           
(define (light-put-body brightness hue)
  (jsexpr->bytes
   (hasheq 'bri brightness 'hue hue 'sat MAX-SAT)))
;; List-of String? - > URL
(define (make-hue-url paths)
  (make-url "http"                ; scheme
            #f                    ; user
            "philips-hue.home"    ; host
            #f                    ; port
            #t                    ; path-absolute?
            ;; path
            (map (λ (s) (make-path/param s null))
                 paths)
            null                  ; query
            #f                    ; fragment
            ))
