(defpackage :alive/range
    (:use :cl)
    (:export :create
             :start
             :end)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/range)


(defun start (range)
    (cdr (assoc :start range)))


(defun end (range)
    (cdr (assoc :end range)))


(defun create (start end)
    (list (cons :start start)
          (cons :end end)))
