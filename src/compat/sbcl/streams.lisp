(defpackage :alive/sbcl/streams
    (:use :cl)
    (:export :input-stream
             :output-stream
             :io-stream
             :eof-p
             :flush-buffer
             :flush-out-buffer
             :add-listener
             :set-listener
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
         (out-closed-p :accessor out-closed-p
                       :initform nil
                       :initarg :out-closed-p)
         (out-eof-p :accessor out-eof-p
                    :initform nil
                    :initarg :out-eof-p)
         (out-lock :accessor out-lock
                   :initform (bt:make-recursive-lock)
                   :initarg :out-lock)
         (out-cond-var :accessor out-cond-var
                       :initform (bt:make-condition-variable)
                       :initarg :out-cond-var)))


(defmethod sb-gray:stream-unread-char ((obj io-stream) ch)
    nil)


(defmethod sb-gray:stream-read-char ((obj io-stream))
    (bt:with-recursive-lock-held ((in-lock obj))
        (flush-out-buffer obj)

        (unless (in-buffer obj)
            (setf (in-buffer obj)
                (funcall (in-listener obj))))

        (if (or (eq :eof (in-buffer obj))
                (zerop (length (in-buffer obj)))
                (not (in-buffer obj)))

            (progn
             (setf (in-buffer obj) nil)
             :eof)

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
        (setf (out-closed-p obj) T)
        (bt:condition-notify (out-cond-var obj))))


(defun flush-out-buffer (obj)
    (let ((str (get-output-stream-string (out-buffer obj))))
        (when (< 0 (length str))
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


(defun end-out-stream (obj)
    (setf (out-eof-p obj) T)
    :eof)


(defun next-out-buffer-line (obj)
    (let* ((pos (position #\linefeed (out-buffer obj)))
           (line nil))

        (if pos
            (progn (setf line (subseq (out-buffer obj) 0 pos))
                   (setf (out-buffer obj) (subseq (out-buffer obj) (+ 1 pos))))
            (progn (setf line (subseq (out-buffer obj) 0))
                   (setf (out-buffer obj) nil)))

        line))


; (defmethod sb-gray:stream-read-line ((obj io-stream))
;     (format T "READ LINE~%")
;     (bt:with-recursive-lock-held ((out-lock obj))
;         (loop :until (or (out-closed-p obj)
;                          (position #\linefeed (out-buffer obj)))
;               :do (bt:condition-wait (out-cond-var obj) (out-lock obj)))

;         (if (zerop (length (out-buffer obj)))
;             (end-out-stream obj)
;             (next-out-buffer-line obj))))


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


(defmethod sb-gray:stream-unread-char ((obj input-stream) ch)
    nil)


(defmethod sb-gray:stream-read-char ((obj input-stream))
    (bt:with-recursive-lock-held ((lock obj))
        (unless (buffer obj)
            (setf (buffer obj)
                (funcall (listener obj))))

        (if (or (eq :eof (buffer obj))
                (zerop (length (buffer obj)))
                (not (buffer obj)))

            (progn
             (setf (buffer obj) nil)
             :eof)

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
