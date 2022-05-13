(defpackage :alive/sbcl/streams
    (:use :cl)
    (:export :input-stream
             :output-stream
             :eof-p
             :flush-buffer
             :add-listener
             :set-listener))

(in-package :alive/sbcl/streams)


(defclass input-stream (sb-gray:fundamental-character-input-stream)
        ((buffer :accessor buffer
                 :initform nil
                 :initarg :buffer)
         (listener :accessor listener
                   :initform nil
                   :initarg :listener)
         (lock :accessor lock
               :initform (bt:make-recursive-lock)
               :initarg :lock)
         (cond-var :accessor cond-var
                   :initform (bt:make-condition-variable)
                   :initarg :cond-var)))


(defmethod sb-gray:stream-read-char ((obj input-stream))
    (bt:with-recursive-lock-held ((lock obj))
        (unless (buffer obj)
            (setf (buffer obj)
                (funcall (listener obj))))

        (if (or (eq :eof (buffer obj))
                (not (buffer obj)))
            :eof
            (let ((ch (elt (buffer obj) 0)))
                (setf (buffer obj)
                    (subseq (buffer obj) 1))

                (when (zerop (length (buffer obj)))
                    (setf (buffer obj) :eof))

                ch))))


(defclass output-stream (sb-gray:fundamental-character-output-stream)
        ((buffer :accessor buffer
                 :initform (make-string-output-stream)
                 :initarg :buffer)
         (listeners :accessor listeners
                    :initform nil
                    :initarg :listeners)
         (closed-p :accessor closed-p
                   :initform nil
                   :initarg :closed-p)
         (eof-p :accessor eof-p
                :initform nil
                :initarg :eof-p)
         (lock :accessor lock
               :initform (bt:make-recursive-lock)
               :initarg :lock)
         (cond-var :accessor cond-var
                   :initform (bt:make-condition-variable)
                   :initarg :cond-var)))


(defmethod stream-element-type ((obj output-stream))
    'character)


(defmethod close ((obj output-stream) &key abort)
    (declare (ignore abort))

    (bt:with-recursive-lock-held ((lock obj))
        (setf (closed-p obj) T)
        (bt:condition-notify (cond-var obj))))


(defun flush-buffer (obj)
    (loop :with str := (get-output-stream-string (buffer obj))
        :for listener :in (listeners obj) :do
        (when (< 0 (length str))
            (funcall listener str))))


(defmethod sb-gray:stream-write-char ((obj output-stream) ch)
    (if (char= #\newline ch)
        (flush-buffer obj)
        (write-char ch (buffer obj))))


(defun add-listener (obj listener)
    (push listener (listeners obj)))


(defun set-listener (obj listener)
    (when obj
        (setf (listener obj) listener)))


(defun end-stream (obj)
    (setf (eof-p obj) T)
    :eof)


(defun next-buffer-line (obj)
    (let* ((pos (position #\linefeed (buffer obj)))
           (line nil))

        (if pos
            (progn (setf line (subseq (buffer obj) 0 pos))
                (setf (buffer obj) (subseq (buffer obj) (+ 1 pos))))
            (progn (setf line (subseq (buffer obj) 0))
                (setf (buffer obj) nil)))

        line))


(defmethod sb-gray:stream-read-line ((obj output-stream))
    (bt:with-recursive-lock-held ((lock obj))
        (loop :until (or (closed-p obj)
                         (position #\linefeed (buffer obj)))
            :do (bt:condition-wait (cond-var obj) (lock obj)))

        (if (zerop (length (buffer obj)))
            (end-stream obj)
            (next-buffer-line obj))))