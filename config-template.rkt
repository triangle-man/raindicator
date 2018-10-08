;;; config.rkt
;;; Template file for locally sensitive data
;;; This file should include definitions for the exported bindings

#lang racket/base

;;; Sensitive local information
;;; ---------------------------
;;; Each installation should create this file separately, rather than making it
;;; public. To obtain an API key, sign up to forecast.io

(require "geo.rkt")

(provide DARKSKY-API-KEY ; String?
         HOME-GEOREF     ; Georef? : Location of your house
         HUE-USERNAME    ; String? : See Hue developer API
         HUE-HOST        ; String? (unused) 
         HUE-LIGHT       ; integer?
         )

(define DARKSKY-API-KEY "fill in api key")
(define HUE-USERNAME "fill in username")

;; Format is (latitude longitude), presumably WGS84
(define HOME-GEOREF (Georef 'latitude 'longitude))

;; Which lights are we using?
(define HUE-HOST "philips-hue.home")
(define HUE-LIGHT 1)






