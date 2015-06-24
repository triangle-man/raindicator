#lang racket

;;; Interface to the forecast.io weather service
;;; See https://developer.forecast.io

(require net/url)
(require net/http-client)
(require json)

;; A Georef identifies a location on the Earth
(struct Georef (latitude longitude))

;; A Forecast represents a weather forecast returned from the forecastio api
(struct Forecast (georef       ; Georef: Requested location
                  timezone     ; Timezone: IANA timezone for requested location
                  currently    ; Datapoint: Current weather conditions at requested location
                  minutely     ; Datablock: Weather minute-by-minute for the next hour
                  hourly       ; Datablock: Weather hour-by-hour for the next two days
                  daily        ; Datablock: Weather day-by-day for the next week
                  alert        ; Alert: Weather alert, if any
                  flags        ; Miscellaneous metadata
                  ))

;; A Timezone represents a timezone
(struct Timezone (name offset))

;; A Datapoint is the summary of the weather at a particular point in time
;; Any of these properties (except time) may be missing, denoted #f
;; Naming convention follows forecastio rather than Racket 
(struct Datapoint
  (time                   ; Number: Unix time (seconds since 1970-01-01 00:00)
   summary                ; String
   icon                   ; String: Text summary of conditions
   sunriseTime            ; Number: Unix time. Only defined on daily datapoints
   sunsetTime             ; Number: Unix time. Only defined on daily datapoints
   moonPhase              ; Number: 0 new-0.5 full-1. Only defined on daily datapoints
   nearestStormDistance   ; Number: In kilometres. Only defined on currently.
   nearestStormBearing    ; Number: 0 North-90 East...
   precipIntensity        ; Number: mm/hour 
   precipIntensityMax     ; Number: Max precipitation on given day. Only on daily datapoints
   precipIntensityMaxTime ; Number: Unix time. Only on daily datapoints
   precipProbability      ; Number: 0-1. Probability of precipitation
   precipType             ; String: One of "rain", "snow", "sleet", or "hail"
   precipAccumulation     ; Number: Snowfall accumulation in cm. Only on daily datapoints
   temperature            ; Number: Temperature in degrees C. Not on daily datapoints
   temperatureMin         ; Number: Only on daily datapoints
   temperatureMinTime     ;
   temperatureMax         ;
   temperatureMaxTime     ;
   apparentTemperatureMin      ; Number: Only on daily datapoints
   apparentTemperatureMinTime  ;
   apparentTemperatureMax      ;
   apparentTemperatureMaxTime  ;
   dewPoint               ; Number: Dew point at time in degrees C
   windSpeed              ; Number: In metres/second
   windBearing            ; Number:
   cloudCover             ; Number: 0 clear-0.4 scattered clouds-0.75 broken cover-1 overcast
   humidity               ; Number: Relative humidity (between 0 and 1)
   pressure               ; Number: In hectopascals (equivalent to millibar)
   visibility             ; Number: In kilometres
   ozone                  ; Number: In Dobson units
   ))

;; A Datablock is a related set of datapoints
(struct Datablock (summary  ; String:
                   icon     ; String:
                   data     ; List-of Datapoint
                   ))

;; An Alert represents a severe weather warning
(struct Alerts (title       ; String: Short text summary
                expires     ; Number: Unix time
                description ; String: Detailed description
                uri         ; String:
                ))

(provide (struct-out Forecast)
         (struct-out Georef)
         (struct-out Timezone)
         (struct-out Datapoint)
         (struct-out Datablock)
         (struct-out Alerts))

(provide get-forecast)

;; String Georef -> Forecast
;; Retrieve a forecast and parse into a Forecast structure
(define (get-forecast api-key georef)
  (parse-forecast (get-forecast/json api-key georef)))


;;---------------------------------------------------------------------------------------------------
;; Internal defines
;;---------------------------------------------------------------------------------------------------

;; Template URL to access the forecast api
;; To create a valid api call, add an api-key and a georef to the path
 (define FORECASTIO
  (make-url "https"                                   ; scheme 
            #f                                        ; user
            "api.forecast.io"                         ; host
            #f                                        ; port
            #t                                        ; path-absolute?
            (list (make-path/param "forecast" null))  ; path, followed by (API georef)
            '((units . "si"))                         ; query
            #f))                                      ; fragment

 ;; jsexpr -> Forecast
 ;; Parse a forecast (as returned by get-forecast/json) into a Forecast structure
(define (parse-forecast forecast)
  (define (extract-part part)
    (hash-ref forecast part #f))
  (Forecast (Georef          (extract-part 'latitude)
                             (extract-part 'longitude))
            (Timezone        (extract-part 'timezone)
                             (extract-part 'offset))
            (parse-datapoint (extract-part 'currently)) 
            (parse-datablock (extract-part 'minutely))  
            (parse-datablock (extract-part 'hourly)) 
            (parse-datablock (extract-part 'daily))  
            (parse-alerts    (extract-part 'alerts))
            (extract-part 'flags))) ; Retain as unparsed jexpr as we don't use the flags

;; String Georef -> jsexpr
;; Retrieve a forecast for a specified georef as JSON
(define (get-forecast/json api-key georef)
  (string->jsexpr (call/input-url (forecast-request-url api-key georef)
                                  get-pure-port
                                  port->string)))

;; Construct a well-formed URL request to the forecastio API
(define (forecast-request-url api-key georef)
  
  (define (georef->string g)
    (string-append (number->string (Georef-latitude g))
                   ","
                   (number->string (Georef-longitude g))))

  (define (append-url u s)
    (struct-copy url
                 u
                 [path (append (url-path u)
                               (list (make-path/param s null)))]))

  (append-url (append-url FORECASTIO api-key)
              (georef->string georef)))


;; Parse various components of a jsexpr forecast

(define (parse-datapoint dp)
  (cond
   [dp   (parse-datapoint/exists dp)]
   [else #f]))

(define (parse-datapoint/exists dp)
  (define (extract-part part) (hash-ref dp part #f))
  (apply Datapoint (map extract-part '(time
                                       summary
                                       icon
                                       sunriseTime
                                       sunsetTime
                                       moonPhase
                                       nearestStormDistance
                                       nearestStormBearing
                                       precipIntensity
                                       precipIntensityMax
                                       precipIntensityMaxTime
                                       precipProbability
                                       precipType
                                       precipAccumulation
                                       temperature
                                       temperatureMin
                                       temperatureMinTime
                                       temperatureMax
                                       temperatureMaxTime
                                       apparentTemperatureMin
                                       apparentTemperatureMinTime
                                       apparentTemperatureMax
                                       apparentTemperatureMaxTime
                                       dewPoint
                                       windSpeed
                                       windBearing
                                       cloudCover
                                       humidity
                                       pressure
                                       visibility
                                       ozone))))

;; A datablock contains a list of datapoints, which we extract by mapping parse-datapoint over the
;; list.
(define (parse-datablock db)
  (cond
   [db   (parse-datablock/exists db)]
   [else #f]))

(define (parse-datablock/exists db)
  (define (extract-part part) (hash-ref db part #f))
  (Datablock (extract-part 'summary)
             (extract-part 'icon)
             (map parse-datapoint (extract-part 'data))))

(define (parse-alerts alerts)
  (cond
   [alerts (parse-alerts/exists alerts)]
   [else   #f]))

(define (parse-alerts/exists alerts)
  (define (extract-part part) (hash-ref alerts part #f))
  (Alerts (extract-part 'title)
          (extract-part 'expires)
          (extract-part 'description)
          (extract-part 'uri)))
