#lang racket

(require net/url)
(require net/http-client)
(require json)

(include "local.rkt")

(provide RED GREEN BLUE)

;; Maximum values

(define MAX-HUE 65535)
(define MAX-BRI 254)
(define MAX-SAT 254)

;; Colours

(define RED       0)
(define GREEN 25500)
(define BLUE  46920)

;; Net

(define HUE-URL
  (make-url "http"                                    ; scheme
            #f                                        ; user
            HUE-HOST                                  ; host
            #f                                        ; port
            #t                                        ; path-absolute?
            (list (make-path/param "api" null))       ; path
            
            
    (define FORECASTIO
  (make-url "https"                                   ; scheme 
            #f                                        ; user
            "api.forecast.io"                         ; host
            #f                                        ; port
            #t                                        ; path-absolute?
            (list (make-path/param "forecast" null))  ; path, followed by (API
                                        ; georef time)
            '((units . "si"))                         ; query
            #f))                                      ; fragment
