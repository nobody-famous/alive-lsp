(defpackage :alive/logger
    (:use :cl)
    (:export *trace*
             *debug*
             *info*
             *error*

             :create
             :init
             :has-level
             :error-msg
             :info-msg
             :debug-msg
             :trace-msg
             :with-logging))

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


(define-condition log-error ()
        ((fmt :accessor fmt
              :initform nil
              :initarg :fmt)
         (args :accessor args
               :initform nil
               :initarg :args))
    (:report (lambda (condition stream) (format stream "LOG ERROR: ~A ~A" (fmt condition) (args condition)))))


(define-condition log-info ()
        ((fmt :accessor fmt
              :initform nil
              :initarg :fmt)
         (args :accessor args
               :initform nil
               :initarg :args))
    (:report (lambda (condition stream) (format stream "LOG INFO: ~A ~A" (fmt condition) (args condition)))))


(define-condition log-debug ()
        ((fmt :accessor fmt
              :initform nil
              :initarg :fmt)
         (args :accessor args
               :initform nil
               :initarg :args))
    (:report (lambda (condition stream) (format stream "LOG DEBUG: ~A ~A" (fmt condition) (args condition)))))


(define-condition log-trace ()
        ((fmt :accessor fmt
              :initform nil
              :initarg :fmt)
         (args :accessor args
               :initform nil
               :initarg :args))
    (:report (lambda (condition stream) (format stream "LOG TRACE: ~A ~A" (fmt condition) (args condition)))))


(defun msg-internal (level fmt &rest args)
    (when *logger*
          (bt:with-recursive-lock-held ((lock *logger*))
              (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (alive/utils:get-timestamp) (level-name level) fmt))
                     (params (concatenate 'list (list (out *logger*) new-fmt) args)))
                  (apply #'format params)))))


(defmacro log-msg (level fmt args)
    `(when (has-level ,level)
           (msg-internal ,level (apply #'format (list nil ,fmt ,@args)))))


(defun has-level (level)
    (when *logger*
          (<= (level *logger*) level)))


(defmacro error-msg (fmt &rest args)
    `(log-msg *error* ,fmt ,args))


(defmacro info-msg (fmt &rest args)
    `(log-msg *info* ,fmt ,args))


(defmacro debug-msg (fmt &rest args)
    `(log-msg *debug* ,fmt ,args))


(defmacro trace-msg (fmt &rest args)
    `(log-msg *trace* ,fmt ,args))


(defmacro with-logging (logger &body body)
    `(let ((*logger* ,logger))
         ,@body))


(defun init (out &optional level)
    (setf *logger* (make-instance 'logger
                       :out out))

    (when level
          (setf (level *logger*) level)))


(defun create (out &optional lvl)
    (let ((logger (make-instance 'logger :out out)))
        (when lvl
              (setf (level logger) lvl))
        logger))
