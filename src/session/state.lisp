(defpackage :alive/session/state
    (:use :cl)
    (:export :add-history
             :add-inspector
             :add-listener
             :create
             :get-file-text
             :get-history-item
             :get-inspector
             :get-sent-msg-callback
             :initialized
             :listener
             :listeners
             :lock
             :msg-handler
             :next-inspector-id
             :next-send-id
             :read-msg
             :rem-inspector
             :running
             :send-msg
             :set-file-text
             :set-initialized
             :set-running
             :set-sent-msg-callback
             :state
             :with-state
             :with-thread-msg)
    (:local-nicknames (:threads :alive/threads)))

(in-package :alive/session/state)


(defparameter *state* nil)


(defstruct listener
    (on-done nil :type (or null function)))


(defstruct state
    (running nil :type boolean)
    (initialized nil :type boolean)

    (files (make-hash-table :test 'equalp) :type hash-table)
    (thread-msgs (make-hash-table :test 'equalp) :type hash-table)
    (sent-msg-callbacks (make-hash-table :test 'equalp) :type hash-table)
    (inspectors (make-hash-table :test 'equalp) :type hash-table)
    (input-cond-vars (make-hash-table :test 'equalp) :type hash-table)

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (listeners nil :type (or null cons))

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex)
    (read-thread nil :type (or null sb-thread:thread))

    (msg-handler nil)
    (read-msg nil)
    (send-msg nil))


(declaim (ftype (function (&key (:msg-handler (function (cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:read-msg (function () (values (or null cons) &optional))))
                          state) create))
(defun create (&key msg-handler send-msg read-msg)
    (make-state :msg-handler msg-handler
                :send-msg send-msg
                :read-msg read-msg))


(declaim (ftype (function () (or null cons)) listeners))
(defun listeners ()
    (unless *state* (error "State not set"))
    (state-listeners *state*))


(declaim (ftype (function () boolean) initialized))
(defun initialized ()
    (unless *state* (error "State not set"))
    (state-initialized *state*))


(declaim (ftype (function () boolean) running))
(defun running ()
    (unless *state* (error "State not set"))
    (state-running *state*))


(declaim (ftype (function (boolean)) set-running))
(defun set-running (value)
    (unless *state* (error "State not set"))
    (setf (state-running *state*) value))


(declaim (ftype (function () sb-thread:mutex) lock))
(defun lock ()
    (unless *state* (error "State not set"))
    (state-lock *state*))


(declaim (ftype (function () T) msg-handler))
(defun msg-handler ()
    (unless *state* (error "State not set"))
    (state-msg-handler *state*))


(declaim (ftype (function (fixnum) (or null function)) get-sent-msg-callback))
(defun get-sent-msg-callback (id)
    (unless *state* (error "State not set"))
    (gethash id (state-sent-msg-callbacks *state*)))


(declaim (ftype (function (fixnum (function (cons) null)) null) set-sent-msg-callback))
(defun set-sent-msg-callback (id cb)
    (unless *state* (error "State not set"))
    (setf (gethash id (state-sent-msg-callbacks *state*)) cb)
    nil)


(declaim (ftype (function () T) read-msg))
(defun read-msg ()
    (unless *state* (error "State not set"))
    (when (state-read-msg *state*)
          (funcall (state-read-msg *state*))))


(declaim (ftype (function (T) null) send-msg))
(defun send-msg (msg)
    (unless *state* (error "State not set"))
    (when (state-send-msg *state*)
          (funcall (state-send-msg *state*) msg)
          nil))


(declaim (ftype (function (T)) add-history))
(defun add-history (item)
    (unless *state* (error "State not set"))
    (setf (elt (state-history *state*) 2) (elt (state-history *state*) 1))
    (setf (elt (state-history *state*) 1) (elt (state-history *state*) 0))
    (setf (elt (state-history *state*) 0) item))


(declaim (ftype (function (integer) T) get-history-item))
(defun get-history-item (index)
    (unless *state* (error "State not set"))
    (when (and (<= 0 index)
               (< index (length (state-history *state*))))
          (elt (state-history *state*) index)))


(declaim (ftype (function (listener)) add-listener))
(defun add-listener (to-add)
    (unless *state* (error "State not set"))
    (push to-add (state-listeners *state*)))


(declaim (ftype (function (boolean)) set-initialized))
(defun set-initialized (value)
    (unless *state* (error "State not set"))
    (setf (state-initialized *state*) value))


(declaim (ftype (function (string string)) set-file-text))
(defun set-file-text (uri text)
    (unless *state* (error "State not set"))
    (setf (gethash uri (state-files *state*)) text))


(declaim (ftype (function (string) (or null string)) get-file-text))
(defun get-file-text (uri)
    (unless *state* (error "State not set"))
    (gethash uri (state-files *state*)))


(declaim (ftype (function () integer) next-send-id))
(defun next-send-id ()
    (unless *state* (error "State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (let ((id (state-send-msg-id *state*)))
            (incf (state-send-msg-id *state*))
            id)))


(declaim (ftype (function () integer) next-inspector-id))
(defun next-inspector-id ()
    (unless *state* (error "State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (let ((id (state-inspector-id *state*)))
            (incf (state-inspector-id *state*))
            id)))


(declaim (ftype (function (integer alive/inspector:inspector)) add-inspector))
(defun add-inspector (id inspector)
    (unless *state* (error "State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (setf (gethash id (state-inspectors *state*))
            inspector)))


(declaim (ftype (function (integer)) rem-inspector))
(defun rem-inspector (id)
    (unless *state* (error "State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (remhash id (state-inspectors *state*))))


(declaim (ftype (function (integer) (or null alive/inspector:inspector)) get-inspector))
(defun get-inspector (id)
    (unless *state* (error "State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (gethash id (state-inspectors *state*))))


(declaim (ftype (function (integer)) save-thread-msg))
(defun save-thread-msg (id)
    (let* ((table (state-thread-msgs *state*))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock *state*))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function () boolean) rem-thread-msg))
(defun rem-thread-msg ()
    (let* ((table (state-thread-msgs *state*))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock *state*))
            (remhash thread-id table))))


(defmacro with-thread-msg ((id) &body body)
    `(progn (unless *state* (error "State not set"))
            (when ,id (save-thread-msg ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (rem-thread-msg)))))


(defmacro with-state (state &body body)
    `(let ((*state* ,state))
         ,@body))
