(defpackage :alive/location
    (:use :cl)
    (:export :create
             :location-p
             :range
             :text-location
             :uri)
    (:local-nicknames (:range :alive/range)))

(in-package :alive/location)


(defun location-p (obj)
    (and (consp obj)
         (assoc :uri obj)
         (assoc :range obj)))


(deftype text-location ()
    `(satisfies location-p))


(declaim (ftype (function (text-location) string) uri))
(defun uri (obj)
    (cdr (assoc :uri obj)))


(declaim (ftype (function (text-location) range:text-range) range))
(defun range (obj)
    (cdr (assoc :range obj)))


(declaim (ftype (function (string range:text-range) cons) create))
(defun create (file range)
    (list (cons :uri file)
          (cons :range range)))
