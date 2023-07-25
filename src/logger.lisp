(defpackage :alive/logger
    (:use :cl)
    (:export *trace*
             *debug*
             *info*
             *error*

             :create
             :init
             :has-level
             :set-level
             :error-msg
             :info-msg
             :debug-msg
             :trace-msg
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


(defun msg-internal (level fmt &rest args)
    (when *logger*
          (bt:with-recursive-lock-held ((lock *logger*))
              (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (alive/utils:get-timestamp) (level-name level) fmt))
                     (params (concatenate 'list (list (out *logger*) new-fmt) args)))
                  (apply #'format params)))))


(defun msg-internal-new (logger level fmt &rest args)
    (when logger
          (bt:with-recursive-lock-held ((lock logger))
              (let* ((new-fmt (format nil "~&[~A][~A] ~A~&" (alive/utils:get-timestamp) (level-name level) fmt))
                     (params (concatenate 'list (list (out logger) new-fmt) args)))
                  (apply #'format params)))))


(defmacro msg (level fmt &rest args)
    `(when (has-level ,level)
           (msg-internal ,level ,fmt ,@args)))


(defun has-level (level)
    (when *logger*
          (<= (level *logger*) level)))


(defun has-level-new (logger level)
    (when logger
          (<= (level logger) level)))


(defun error-msg (logger fmt &rest args)
    (when (has-level-new logger *error*)
          (apply #'msg-internal-new (concatenate 'list (list logger *error* fmt) args))))


(defun info-msg (logger fmt &rest args)
    (when (has-level-new logger *info*)
          (apply #'msg-internal-new (concatenate 'list (list logger *info* fmt) args))))


(defun debug-msg (logger fmt &rest args)
    (when (has-level-new logger *debug*)
          (apply #'msg-internal-new (concatenate 'list (list logger *debug* fmt) args))))


(defun trace-msg (logger fmt &rest args)
    (when (has-level-new logger *trace*)
          (apply #'msg-internal-new (concatenate 'list (list logger *trace* fmt) args))))


(defun set-level (level)
    (when *logger*
          (setf (level *logger*) level)))


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
