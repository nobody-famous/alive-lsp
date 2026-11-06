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


(defun start (range)
    (cdr (assoc :start range)))


(defun end (range)
    (cdr (assoc :end range)))


(defun create (start end)
    (list (cons :start start)
          (cons :end end)))
