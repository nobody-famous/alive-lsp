(defpackage :alive/location
    (:use :cl)
    (:export :create
             :text-location)
    (:local-nicknames (:range :alive/range)))

(in-package :alive/location)


(defun location-p (obj)
    (and (consp obj)
         (stringp (first obj))
         (typep (second obj) 'range:text-range)))


(deftype text-location ()
    `(satisfies location-p))


(declaim (ftype (function (string range:text-range) list) create))
(defun create (uri range)
    (list uri range))
