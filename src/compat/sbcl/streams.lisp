(defpackage :alive/sbcl/streams
    (:use :cl)
    (:export :io-stream
             :flush-out-buffer
             :set-in-listener
             :set-out-listener))

(in-package :alive/sbcl/streams)


(defclass io-stream (sb-gray:fundamental-character-input-stream sb-gray:fundamental-character-output-stream)
        ((in-buffer :accessor in-buffer
                    :initform nil
                    :initarg :in-buffer)
         (in-listener :accessor in-listener
                      :initform nil
                      :initarg :in-listener)
         (in-lock :accessor in-lock
                  :initform (bt:make-recursive-lock)
                  :initarg :in-lock)
         (in-cond-var :accessor in-cond-var
                      :initform (bt:make-condition-variable)
                      :initarg :in-cond-var)

         (out-buffer :accessor out-buffer
                     :initform (make-string-output-stream)
                     :initarg :out-buffer)
         (out-listener :accessor out-listener
                       :initform nil
                       :initarg :out-listener)
         (out-lock :accessor out-lock
                   :initform (bt:make-recursive-lock)
                   :initarg :out-lock)
         (out-cond-var :accessor out-cond-var
                       :initform (bt:make-condition-variable)
                       :initarg :out-cond-var)))


(defmethod sb-gray:stream-unread-char ((obj io-stream) ch)
    (if (eq :eof (in-buffer obj))
        (setf (in-buffer obj) (princ-to-string ch))
        (setf (in-buffer obj) (format nil "~C~A" ch (in-buffer obj))))

    nil)


(defmethod sb-gray:stream-read-char ((obj io-stream))
    (bt:with-recursive-lock-held ((in-lock obj))
        (flush-out-buffer obj)

        (when (and (in-listener obj)
                   (not (in-buffer obj)))
              (setf (in-buffer obj)
                  (funcall (in-listener obj))))

        (if (or (eq :eof (in-buffer obj))
                (zerop (length (in-buffer obj))))

            (progn (setf (in-buffer obj) nil)
                   #\newline)

            (let ((ch (elt (in-buffer obj) 0)))
                (setf (in-buffer obj)
                    (subseq (in-buffer obj) 1))

                (when (zerop (length (in-buffer obj)))
                      (setf (in-buffer obj) :eof))

                ch))))


(defmethod stream-element-type ((obj io-stream))
    'character)


(defmethod close ((obj io-stream) &key abort)
    (declare (ignore abort))

    (bt:with-recursive-lock-held ((out-lock obj))
        (bt:condition-notify (out-cond-var obj))))


(defun flush-out-buffer (obj)
    (let ((str (get-output-stream-string (out-buffer obj))))
        (when (and (out-listener obj)
                   (< 0 (length str)))
              (funcall (out-listener obj) str))))


(defmethod sb-gray:stream-write-char ((obj io-stream) ch)
    (if (char= #\newline ch)
        (flush-out-buffer obj)
        (write-char ch (out-buffer obj))))


(defun set-in-listener (obj listener)
    (when obj
          (setf (in-listener obj) listener)))


(defun set-out-listener (obj listener)
    (when obj
          (setf (out-listener obj) listener)))
