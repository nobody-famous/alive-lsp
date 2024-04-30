(defpackage :alive/position
    (:use :cl)
    (:export :create
             :line
             :col
             :less-than
             :less-or-equal
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/position)


(defun position-p (obj)
    (and (consp obj)
         (assoc :line obj)
         (assoc :character obj)))


(deftype pos ()
    `(satisfies position-p))


(declaim (ftype (function (pos) fixnum) line))
(defun line (obj)
    (cdr (assoc :line obj)))


(declaim (ftype (function (pos) fixnum) col))
(defun col (obj)
    (cdr (assoc :character obj)))


(declaim (ftype (function (pos pos) boolean) less-than))
(defun less-than (pos1 pos2)
    (let ((line1 (cdr (assoc :line pos1)))
          (col1 (cdr (assoc :character pos1)))
          (line2 (cdr (assoc :line pos2)))
          (col2 (cdr (assoc :character pos2))))

        (cond ((< line1 line2) T)
              ((< line2 line1) NIL)
              (T (< col1 col2)))))


(declaim (ftype (function (pos pos) boolean) less-or-equal))
(defun less-or-equal (pos1 pos2)
    (let ((line1 (cdr (assoc :line pos1)))
          (col1 (cdr (assoc :character pos1)))
          (line2 (cdr (assoc :line pos2)))
          (col2 (cdr (assoc :character pos2))))

        (or (less-than pos1 pos2)
            (and (= line1 line2)
                 (= col1 col2)))))


(declaim (ftype (function (fixnum fixnum) cons) create))
(defun create (line col)
    (list (cons :line line)
          (cons :character col)))
