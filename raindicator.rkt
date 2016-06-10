#lang racket

(require net/url)
(require net/http-client)
(require json)

(require "forecastio.rkt")
(include "local.rkt")

(define MAXPRECIPITATION (* 25.4 0.4)) ; Intended as a reference value,
                                       ; corresponding to "heavy" rain. The
                                       ; actual number from forecast.io may be
                                       ; higher than this

(define DECAY-CONSTANT 20) ; In minutes, for the NPV calculation

;; List-of (time, rainfall), number -> number, number
;; Convert the minute-by-minute rainfall for the next hour into a pair of
;; numbers: the "NPV" of the rain, and the peak rainfall
;; NPV is normalised based on which datapoints are actually available

(define (summarise-rain precipitation decay-constant)
  (define times (map car precipitation))
  (define rains (map cdr precipitation))
  (define norms (present-values times decay-constant))
  (values
   (/ (apply + (map * norms rains))
      (apply + norms))
   (apply max rains)))
  
(define (present-values times decay-constant)
  (map
   (λ (x) (exp (- (/ x decay-constant))))
   times))




