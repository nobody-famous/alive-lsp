(defpackage :alive/logger
    (:use :cl)
    (:export *trace*
             *debug*
             *info*
             *error*

             :init
             :has-level
             :set-level
             :msg))

(in-package :alive/logger)


(defparameter *trace* 0)
(defparameter *debug* 1)
(defparameter *info* 2)
(defparameter *error* 3)

(defparameter *logger* NIL)

(defparameter *level-names* (make-hash-table))


(setf (gethash *trace* *level-names*) "TRACE")
(setf (gethash *debug* *level-names*) "DEBUG")
(setf (gethash *info* *level-names*) "INFO")
(setf (gethash *error* *level-names*) "ERROR")


(defclass logger ()
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


(defun msg (level fmt &rest args)
    (when *logger*
          (bt:with-recursive-lock-held ((lock *logger*))
              (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (get-timestamp) (level-name level) fmt))
                     (params (concatenate 'list (list (out *logger*) new-fmt) args)))
                  (apply #'format params)))))


(defun has-level (level)
    (when *logger*
          (<= (level *logger*) level)))


(defun set-level (level)
    (when *logger*
          (setf (level *logger*) level)))


(defun init (out &optional level)
    (setf *logger* (make-instance 'logger
                       :out out))

    (when level
          (setf (level *logger*) level)))