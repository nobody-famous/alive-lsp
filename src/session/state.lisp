(defpackage :alive/session/state
    (:use :cl)
    (:export :add-history
             :add-inspector
             :add-listener
             :create
             :create-listener
             :get-file-text
             :get-files
             :get-history-item
             :get-inspector
             :get-sent-msg-callback
             :initialized
             :listener
             :listeners
             :lock
             :next-inspector-id
             :next-send-id
             :next-thread-id
             :rem-inspector
             :rem-thread-msg
             :running
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


(declaim (ftype (function (function) listener) create-listener))
(defun create-listener (done-fn)
    (make-listener :on-done done-fn))


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
    (read-thread nil :type (or null sb-thread:thread)))


(declaim (ftype (function () state) create))
(defun create ()
    (make-state))


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


(defmacro lock ((mutex) &body body)
    `(progn (unless *state* (error "State not set"))
            (let ((,mutex (state-lock *state*)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(declaim (ftype (function (fixnum) (or null function)) get-sent-msg-callback))
(defun get-sent-msg-callback (id)
    (unless *state* (error "State not set"))
    (gethash id (state-sent-msg-callbacks *state*)))


(declaim (ftype (function (fixnum (function (cons) hash-table)) null) set-sent-msg-callback))
(defun set-sent-msg-callback (id cb)
    (unless *state* (error "State not set"))
    (setf (gethash id (state-sent-msg-callbacks *state*)) cb)
    nil)


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


(declaim (ftype (function () hash-table) get-files))
(defun get-files ()
    (unless *state* (error "State not set"))
    (state-files *state*))


(defmacro next-id (fn)
    `(progn (unless *state* (error "State not set"))
            (bt:with-recursive-lock-held ((state-lock *state*))
                (let ((id (,fn *state*)))
                    (incf (,fn *state*))
                    id))))


(declaim (ftype (function () integer) next-send-id))
(defun next-send-id ()
    (next-id state-send-msg-id))


(declaim (ftype (function () integer) next-inspector-id))
(defun next-inspector-id ()
    (next-id state-inspector-id))


(declaim (ftype (function () integer) next-thread-id))
(defun next-thread-id ()
    (next-id state-thread-name-id))


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
