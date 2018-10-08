#lang racket/base

(require "minutely.rkt")
(require "hue.rkt")
(require "local.rkt")

;; Notes on normalisation
;;
;; Rainfall is in mm/hour. Roughly, anything less
;; than 2.5 is "light", 2.5-5 is "moderate", and 5 - 7.5 is "heavy". More than
;; 7.5 is a real storm
(define MAX-RAIN 7.5)

;; Main: update the light every 5 minutes
(module+ main
  (let loop ()
    (update-raindicator!)
    (sleep 300)
    (loop)))


 ;; Get rain forecast and set colour appropriately. 
(define (update-raindicator!)
  (define-values (high average)
    (imminent-rainfall/summary DARKSKY-API-KEY HOME-GEOREF))
  ;; (printf "High: ~a Average: ~a~n" high average)
  (update-light! high average))

;; If no rain, then set the light to minimum brightness white;
;; otherwise set to the appropriate colour
;; high in mm/hour
;; average in mm/hour
(define (update-light! high average)
  (if (eq? high 0)
      (light-set-bri-ct! HUE-LIGHT 1 250 HUE-USERNAME)
      (let-values ([(hue bri) (rain->light high average)])
        (light-set-bri-hue! HUE-LIGHT bri hue HUE-USERNAME))))

;; rain->light : high average -> values colour brightness
;; Convert max and average rainfall into a brightness and colour
;; high average -> values colour brightness 
;; high and average are in mm/hour
;; brightness is zero to MAX-BRI (imported from hue.rkt)
;; colour is RED to BLUE (imported from hue.rkt)
(define (rain->light high average)
  (define-values (normalised-high normalised-average)
    (normalise-rain high average))
  (values
   (+ RED (* normalised-high (- BLUE RED)))
   (ceiling (* normalised-average MAX-BRI))))

;; normalise-rain : high average -> values high average
;; Normalise high and average rainfall
;; High is normalised to 0 - 1 where 1 means greater than MAX-RAIN.
;; Average is normalised to 0 - 1 where 1 means whatever high is for the next hour
(define (normalise-rain high average)
  (define normalised-high
    (min (/ high MAX-RAIN) 1.0))
  (define normalised-average
    (/ average high))
  (values normalised-high normalised-average))







