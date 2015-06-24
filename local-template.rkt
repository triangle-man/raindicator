;;; Template file for locally sensitive data
;;; This file should export two bindings:
;;; 1. API-KEY, a string
;;; 2. GEOREF, your location, as a Georef

#lang racket

(require "forecastio.rkt")

;;; Sensitive local information.

;;; Each installation should create this file separately, rather than making it
;;; public. To obtain an API key, sign up to forecast.io

(provide API-KEY GEOREF)

(define API-KEY "")

;; Format is (latitude longitude), presumably WGS84
(define GEOREF (Georef ))




