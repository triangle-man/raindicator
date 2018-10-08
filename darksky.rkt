#lang racket/base

;;; Interface to the darksky weather service
;;; See https://darksky.net/dev
;;; Updated: 2018-09

(require racket/port)
(require net/url)
(require net/http-client)
(require json)

(require "geo.rkt")

(provide (struct-out Forecast)    
         (struct-out Datapoint)
         (struct-out Datablock)
         (struct-out Alert))

(provide get-forecast           ; String Georef -> Forecast
         )

;; A Forecast represents a weather forecast returned from the forecastio api
(struct Forecast (location     ; Georef: Requested location
                  timezone     ; Timezone: IANA timezone for requested location
                  currently    ; Datapoint: Current weather conditions at requested location
                  minutely     ; Datablock: Weather minute-by-minute for the next hour
                  hourly       ; Datablock: Weather hour-by-hour for the next two days
                  daily        ; Datablock: Weather day-by-day for the next week
                  alerts       ; list-of Alert: Weather alerts, if any
                  flags        ; Miscellaneous metadata
                  ) #:transparent)

;; A Datablock is a related set of datapoints
(struct Datablock (summary  ; String:
                   icon     ; String:
                   data     ; List-of Datapoint
                   ) #:transparent)

;; A Datapoint is the summary of the weather at a particular point in time
;; Any of these properties (except time) may be missing, which is denoted #f
;; Naming convention follows forecastio rather than Racket 
(struct Datapoint
  (time                   ; Number: Unix time (seconds since 1970-01-01 00:00)
   summary                ; String: Summary of conditions, human readable
   icon                   ; String: Summary of conditions, machine readable
   sunriseTime            ; Number: Unix time. Only defined on daily datapoints
   sunsetTime             ; Number: Unix time. Only defined on daily datapoints
   moonPhase              ; Number: 0 new-0.5 full-1. Only defined on daily datapoints
   nearestStormDistance   ; Number: In kilometres. Only defined on currently.
   nearestStormBearing    ; Number: 0 North-90 East...
   precipIntensity        ; Number: mm/hour (heavy precipitation is around 10 mm/hour)
   precipIntensityError   ; Number: std. dev. of precipIntensity
   precipIntensityMax     ; Number: Max precipitation on given day. Only on daily datapoints
   precipIntensityMaxTime ; Number: Unix time. Only on daily datapoints
   precipProbability      ; Number: 0-1. Probability of precipitation
   precipType             ; String: One of "rain", "snow", or "sleet"
   precipAccumulation     ; Number: Snowfall accumulation in cm. Only on daily datapoints
   temperature            ; Number: Temperature in degrees C. Not on daily datapoints
   temperatureLow         ; Number: Only on daily datapoints
   temperatureLowTime     ;
   temperatureHigh        ;
   temperatureHighTime    ;
   apparentTemperature    ; Number: Not on daily datatpoints
   apparentTemperatureLow      ; Number: Only on daily datapoints
   apparentTemperatureLowTime  ; "
   apparentTemperatureHigh     ; "
   apparentTemperatureHighTime ; "
   dewPoint               ; Number: Dew point at time in degrees C
   windSpeed              ; Number: In metres/second
   windBearing            ; Number: Direction from which wind comes (E is 90 etc)
   windGust               ; Number:
   windGustTime           ; Number:
   cloudCover             ; Number: 0 clear-0.4 scattered clouds-0.75 broken cover-1 overcast
   humidity               ; Number: Relative humidity (between 0 and 1)
   pressure               ; Number: In hectopascals (equivalent to millibar)
   visibility             ; Number: In kilometres
   ozone                  ; Number: In Dobson units
   uvIndex                ; Number
   uvIndexTime            ; Number
   ) #:transparent)

;; An Alert represents a severe weather warning
(struct Alert (title       ; String: Short text summary
               severity    ; String: Either "advisory", "watch", or "warning"
               time        ; Number: Time alert was issued
               expires     ; Number: Unix time
               regions     ; List-of String: Names of regions covered by alert
               description ; String: Detailed description
               uri         ; String:
               ) #:transparent)

;;---------------------------------------------------------------------------------------------------
;; Constants
;;---------------------------------------------------------------------------------------------------

;; Template URL to access the forecast api
;; To create a valid api call, add an api-key and a georef to the path
 (define DARKSKY-URL
  (make-url "https"                                   ; scheme 
            #f                                        ; user
            "api.darksky.net"                         ; host
            #f                                        ; port
            #t                                        ; path-absolute?
            (list (make-path/param "forecast" null))  ; path, followed by (API-KEY
                                                      ; georef time)
            '((units . "si"))                         ; query
            #f))                                      ; fragment

;;---------------------------------------------------------------------------------------------------
;; Interface
;;---------------------------------------------------------------------------------------------------

;; String Georef -> Forecast
;; Retrieve a forecast and parse into a Forecast structure
(define (get-forecast api-key georef #:time [time #f])
  (parse-forecast (get-forecast/json api-key georef time)))

;; String Georef -> jsexpr
;; Retrieve a forecast for a specified georef (and, optionally, time) as JSON
(define (get-forecast/json api-key georef time)
  (string->jsexpr
   (call/input-url
    (forecast-request-url api-key georef time)
    get-pure-port
    port->string)))


;;---------------------------------------------------------------------------------------------------
;; Internal defines
;;---------------------------------------------------------------------------------------------------

 ;; jsexpr -> Forecast
 ;; Parse a forecast (as returned by get-forecast/json) into a Forecast structure
(define (parse-forecast forecast)
  (define (extract-part part)
    (hash-ref forecast part #f))
  (Forecast (Georef          (extract-part 'latitude) (extract-part 'longitude))
            (Timezone        (extract-part 'timezone)) ; 'offset is deprecated
            (parse-datapoint (extract-part 'currently)) 
            (parse-datablock (extract-part 'minutely))  
            (parse-datablock (extract-part 'hourly)) 
            (parse-datablock (extract-part 'daily))  
            (parse-alerts    (extract-part 'alerts))
            (extract-part 'flags))) ; Retain as unparsed jexpr

;; Construct a well-formed URL request to the forecastio API
(define (forecast-request-url api-key georef time)
  (define georef/string
    (string-append (number->string (Georef-latitude georef))
                   ","
                   (number->string (Georef-longitude georef))))
  (define path
    (if time
        (string-append georef/string "," (number->string time))
        georef/string))
     
  (define (append-url u s)
    (struct-copy url
                 u
                 [path (append (url-path u)
                               (list (make-path/param s null)))]))
  (append-url (append-url DARKSKY-URL api-key) path))
                  
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
                                       precipIntensityError
                                       precipIntensityMax
                                       precipIntensityMaxTime
                                       precipProbability
                                       precipType
                                       precipAccumulation
                                       temperature
                                       temperatureLow
                                       temperatureLowTime
                                       temperatureHigh
                                       temperatureHighTime
                                       apparentTemperature
                                       apparentTemperatureLow
                                       apparentTemperatureLowTime
                                       apparentTemperatureHigh
                                       apparentTemperatureHighTime
                                       dewPoint
                                       windSpeed
                                       windBearing
                                       windGust
                                       windGustTime
                                       cloudCover
                                       humidity
                                       pressure
                                       visibility
                                       ozone
                                       uvIndex
                                       uvIndexTime))))

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
  (map parse-alert alerts))

(define (parse-alert alert)
  (define (extract-part part) (hash-ref alert part #f))
  (Alert (extract-part 'title)
         (extract-part 'severity)
         (extract-part 'time)
         (extract-part 'expires)
         (extract-part 'regions)
         (extract-part 'description)
         (extract-part 'uri)))
