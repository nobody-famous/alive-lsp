(defpackage :alive/range
    (:use :cl)
    (:export :create
             :start
             :end
             :text-range)
    (:local-nicknames (:pos :alive/position)))

(in-package :alive/range)


(defun range-p (obj)
    (and (consp obj)
         (assoc :start obj)
         (assoc :end obj)))


(deftype text-range ()
    `(satisfies range-p))


(declaim (ftype (function (text-range) pos:text-position) start))
(defun start (range)
    (cdr (assoc :start range)))


(declaim (ftype (function (text-range) pos:text-position) end))
(defun end (range)
    (cdr (assoc :end range)))


(declaim (ftype (function (pos:text-position pos:text-position) text-range) create))
(defun create (start end)
    (list (cons :start start)
          (cons :end end)))
