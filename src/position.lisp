(defpackage :alive/position
    (:use :cl)
    (:export :create
             :line
             :col
             :less-than
             :less-than-new
             :less-or-equal
             :less-or-equal-new
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/position)


(defun line (obj)
    (cdr (assoc :line obj)))


(defun col (obj)
    (cdr (assoc :col obj)))


(defun less-than (pos1 pos2)
    (let ((line1 (cdr (assoc :line pos1)))
          (col1 (cdr (assoc :col pos1)))
          (line2 (cdr (assoc :line pos2)))
          (col2 (cdr (assoc :col pos2))))

        (cond ((< line1 line2) T)
              ((< line2 line1) NIL)
              (T (< col1 col2)))))


(defun less-or-equal (pos1 pos2)
    (let ((line1 (cdr (assoc :line pos1)))
          (col1 (cdr (assoc :col pos1)))
          (line2 (cdr (assoc :line pos2)))
          (col2 (cdr (assoc :col pos2))))

        (format T "pos:less-or-equal ~A ~A ~A ~A ~A ~A~%" pos1 pos2 line1 col1 line2 col2)
        (or (less-than pos1 pos2)
            (and (= line1 line2)
                 (= col1 col2)))))


(defun create (line col)
    (list (cons :line line)
          (cons :col col)))


(defun from-wire (fields)
    (mapcar (lambda (field)
                (cond ((eq :character (car field)) (cons :col (cdr field)))
                      (T field)))
            fields))
