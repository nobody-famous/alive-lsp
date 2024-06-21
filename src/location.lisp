(defpackage :alive/location
    (:use :cl)
    (:export :create
             :list-of-location
             :text-location)
    (:local-nicknames (:range :alive/range)))

(in-package :alive/location)


(defun location-p (obj)
    (and (consp obj)
         (stringp (cdr (assoc :uri obj)))
         (typep (cdr (assoc :range obj)) 'range:text-range)))


(deftype text-location ()
    `(satisfies location-p))


(defun list-of-location-p (data)
    (or (not data)
        (and (consp data)
             (every #'location-p data))))


(deftype list-of-location ()
    `(satisfies list-of-location-p))


(declaim (ftype (function (string (or null range:text-range)) text-location) create))
(defun create (uri range)
    (list (cons :uri uri)
          (cons :range range)))
