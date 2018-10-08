#lang racket/base

;;; minutely.rkt
;;; Calculations on the near-term (next hour) precipitation forecast

(require racket/list)
(require racket/math)
(require "darksky.rkt")

(provide imminent-rainfall ; Return a list of pairs (minute, precipition)  
         imminent-rainfall/summary ; Return a summary as two values (max, average)
         )

;; 3 in/hour is very heavy rain
(define MAX-INTENSITY (* 3.0 2.5))
(define DECAY-CONSTANT (/ 30.0 (log 2)))
(define DECAY-PROFILE
  (map (λ (t) (exp (- (/ t DECAY-CONSTANT))))
       (range 0 61)))

;;---------------------------------------------------------------------------------------------------
;; Interface
;;---------------------------------------------------------------------------------------------------

;; Forecast -> List-of (Number . Number) or #f
;;
;; Extract from a Forecast a list of pairs of the time from now (in minutes,
;; possibly including zero) and the rainfall at that time in mm/hour. The list
;; is not necessarily of length 60.
;;
;; The boundary between "Light" and "Medium" on the darksky app is about 2.5
;; mm/hour.

(define (imminent-rainfall api-key georef)
  (define fc (get-forecast api-key georef))
  ;; minutely : List-of Datapoint
  (define minutely (Datablock-data (Forecast-minutely fc)))
  (define now (Datapoint-time (Forecast-currently fc)))
  (if (and minutely now)
      (extract-precipitation minutely now)
      #f)
  )

;; Forecast -> Values Number Number
;;
;; The first value returned is the maximum rainfall in the next
;; hour.
;;
;; The second value returned is the average rainfall rate over the hour, weighted more
;; heavily for times closer to now, scaled so that the average of a constant
;; rate is that rate.
;;
(define (imminent-rainfall/summary api-key georef)
  (summarise-rainfall (imminent-rainfall api-key georef)))

;; 
;;---------------------------------------------------------------------------------------------------
;; Internal defines
;;---------------------------------------------------------------------------------------------------

;; (List-of Datapoint) Number? -> List-of (Number? . Number?)
(define (extract-precipitation minutely now)
  (define precipitation
    (map (λ (m)
           (cons (exact-round
                  (/ (- (Datapoint-time m) now) 60.0)) ; Convert to minutes from now
                 (Datapoint-precipIntensity m)))
         minutely))
    ;; Remove data which appears to be invalid
  (filter (λ (m)
            (let ([t (car m)]
                  [p (cdr m)])
              (and (>= t  0)       ; times between 0 
                   (<  t 62)       ; and 62 
                   (>= p  0))))    ; and positive precipitation
          precipitation))

;; List-of (Number? . Number?) or #f -> Values Number? Number? or #f #f
(define (summarise-rainfall rain-profile)
  (if (not rain-profile)
      (values #f #f)
      (values (highest rain-profile) (weighted-average rain-profile))))

(define (highest rains)
  (apply max (map cdr rains)))

(define (weighted-average rain-profile)
  ;; indices is a list of the times at which we have valid rainfall data
  (define indices (map car rain-profile))  
  (define rains (map cdr rain-profile))
  (define weights (map (λ (t) (list-ref DECAY-PROFILE t)) indices))
  (if (eq? (length indices) 0)
      0.0
      (/ (apply + (map * weights rains))
         (apply + weights))))


