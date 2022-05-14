(defpackage :alive/logger
            (:use :cl)
            (:export *trace*
                     *debug*
                     *info*
                     *error*

                     :create
                     :trace-msg
                     :debug-msg
                     :info-msg
                     :error-msg
                     :set-level))

(in-package :alive/logger)


(defparameter *trace* 0)
(defparameter *debug* 1)
(defparameter *info* 2)
(defparameter *error* 3)


(defparameter *level-names* (make-hash-table))


(setf (gethash *trace* *level-names*) "TRACE")
(setf (gethash *debug* *level-names*) "DEBUG")
(setf (gethash *info* *level-names*) "INFO")
(setf (gethash *error* *level-names*) "ERROR")


(defclass config ()
          ((level :accessor level
                  :initform *error*
                  :initarg :level)
           (lock :accessor lock
                 :initform (bt:make-recursive-lock)
                 :initarg :lock)
           (out :accessor out
                :initform nil
                :initarg :out)))


(defun level-name (level)
    (gethash level *level-names* "??"))


(defun get-timestamp ()
    (multiple-value-bind (sec minute hour day month year)
            (decode-universal-time (get-universal-time))
        (format nil "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d" month day year hour minute sec)))


(defun msg (logger level fmt &rest args)
    (bt:with-recursive-lock-held ((lock logger))
                                 (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (get-timestamp) (level-name level) fmt))
                                        (params (concatenate 'list (list (out logger) new-fmt) args)))
                                     (apply #'format params))))


(defun trace-msg (logger fmt &rest rest)
    (when (<= (level logger) *trace*)
        (apply #'msg logger *trace* fmt rest)))


(defun debug-msg (logger fmt &rest rest)
    (when (<= (level logger) *debug*)
        (apply #'msg logger *debug* fmt rest)))


(defun info-msg (logger fmt &rest rest)
    (when (<= (level logger) *info*)
        (apply #'msg logger *info* fmt rest)))


(defun error-msg (logger fmt &rest rest)
    (when (<= (level logger) *error*)
        (apply #'msg logger *error* fmt rest)))


(defun set-level (logger level)
    (setf (level logger) level))


(defun create (out &optional level)
    (let ((obj (make-instance 'config
                              :out out)))
        (when level (setf (level obj) level))
        obj))