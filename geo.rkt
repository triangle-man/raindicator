#lang racket/base

;; Geospatial data structures

(provide (struct-out Georef)
         (struct-out Timezone))

;; A Georef identifies a location on the Earth
(struct Georef (latitude longitude) #:transparent)

;; A Timezone represents a timezone
;; DarkSky has deprecated the `offset` parameter, so we no longer use it
(struct Timezone (name) #:transparent)

