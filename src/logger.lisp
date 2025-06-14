(defpackage :alive/logger
    (:use :cl)
    (:export *trace*
             *debug*
             *info*
             *error*

             :create
             :debug-msg
             :error-msg
             :has-level
             :info-msg
             :logger
             :trace-msg))

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


(defun msg-internal (logger level fmt &rest args)
    (when logger
          (bt:with-recursive-lock-held ((lock logger))
              (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (alive/utils:get-timestamp) (level-name level) fmt))
                     (params (concatenate 'list (list (out logger) new-fmt) args)))
                  (apply #'format params)))))


(defun has-level (logger level)
    (when logger
          (<= (level logger) level)))


(defmacro log-msg (logger level fmt args)
    `(when (has-level ,logger ,level)
           (msg-internal ,logger ,level (apply #'format (list nil ,fmt ,@args)))))


(defmacro error-msg (logger fmt &rest args)
    `(log-msg ,logger *error* ,fmt ,args))


(defmacro info-msg (logger fmt &rest args)
    `(log-msg ,logger *info* ,fmt ,args))


(defmacro debug-msg (logger fmt &rest args)
    `(log-msg ,logger *debug* ,fmt ,args))


(defmacro trace-msg (logger fmt &rest args)
    `(log-msg ,logger *trace* ,fmt ,args))


(defun create (out &optional lvl)
    (let ((logger (make-instance 'logger :out out)))
        (when lvl
              (setf (level logger) lvl))
        logger))
