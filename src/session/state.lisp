(defpackage :alive/session/state
    (:use :cl)
    (:export :add-history
             :add-inspector
             :add-listener
             :get-file-text
             :get-history-item
             :get-inspector
             :initialized
             :listener
             :listeners
             :lock
             :next-inspector-id
             :next-send-id
             :rem-inspector
             :running
             :set-file-text
             :set-initialized
             :set-running
             :state
             :with-thread-msg)
    (:local-nicknames (:threads :alive/threads)))

(in-package :alive/session/state)


(defstruct listener
    (on-done nil :type (or null function)))


(defstruct state
    (running nil :type boolean)
    (initialized nil :type boolean)

    (files (make-hash-table :test 'equalp) :type hash-table)
    (thread-msgs (make-hash-table :test 'equalp) :type hash-table)
    (send-msg-callbacks (make-hash-table :test 'equalp) :type hash-table)
    (inspectors (make-hash-table :test 'equalp) :type hash-table)
    (input-cond-vars (make-hash-table :test 'equalp) :type hash-table)

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (listeners nil :type (or null cons))

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex)
    (read-thread nil :type (or null sb-thread:thread)))


(declaim (ftype (function (state) (or null cons)) listeners))
(defun listeners (state)
    (state-listeners state))


(declaim (ftype (function (state) boolean) initialized))
(defun initialized (state)
    (state-initialized state))


(declaim (ftype (function (state) boolean) running))
(defun running (state)
    (state-running state))


(declaim (ftype (function (state boolean)) set-running))
(defun set-running (state value)
    (setf (state-running state) value))


(declaim (ftype (function (state) sb-thread:mutex) lock))
(defun lock (state)
    (state-lock state))


(declaim (ftype (function (state T)) add-history))
(defun add-history (obj item)
    (setf (elt (state-history obj) 2)
        (elt (state-history obj) 1))
    (setf (elt (state-history obj) 1)
        (elt (state-history obj) 0))
    (setf (elt (state-history obj) 0)
        item))


(declaim (ftype (function (state integer) T) get-history-item))
(defun get-history-item (obj index)
    (when (and (<= 0 index)
               (< index (length (state-history obj))))
          (elt (state-history obj) index)))


(declaim (ftype (function (state listener)) add-listener))
(defun add-listener (obj to-add)
    (push to-add (state-listeners obj)))


(declaim (ftype (function (state boolean)) set-initialized))
(defun set-initialized (obj value)
    (setf (state-initialized obj) value))


(declaim (ftype (function (state string string)) set-file-text))
(defun set-file-text (obj uri text)
    (setf (gethash uri (state-files obj)) text))


(declaim (ftype (function (state string) (or null string)) get-file-text))
(defun get-file-text (obj uri)
    (gethash uri (state-files obj)))


(declaim (ftype (function (state) integer) next-send-id))
(defun next-send-id (obj)
    (bt:with-recursive-lock-held ((state-lock obj))
        (let ((id (state-send-msg-id obj)))
            (incf (state-send-msg-id obj))
            id)))


(declaim (ftype (function (state) integer) next-inspector-id))
(defun next-inspector-id (obj)
    (bt:with-recursive-lock-held ((state-lock obj))
        (let ((id (state-inspector-id obj)))
            (incf (state-inspector-id obj))
            id)))


(declaim (ftype (function (state integer alive/inspector:inspector)) add-inspector))
(defun add-inspector (obj id inspector)
    (bt:with-recursive-lock-held ((state-lock obj))
        (setf (gethash id (state-inspectors obj))
            inspector)))


(declaim (ftype (function (state integer)) rem-inspector))
(defun rem-inspector (obj id)
    (bt:with-recursive-lock-held ((state-lock obj))
        (remhash id (state-inspectors obj))))


(declaim (ftype (function (state integer) (or null alive/inspector:inspector)) get-inspector))
(defun get-inspector (obj id)
    (bt:with-recursive-lock-held ((state-lock obj))
        (gethash id (state-inspectors obj))))


(declaim (ftype (function (state integer)) save-thread-msg))
(defun save-thread-msg (state id)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function (state)) rem-thread-msg))
(defun rem-thread-msg (state)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (remhash thread-id table))))


(defmacro with-thread-msg ((state id) &body body)
    `(progn (when ,id (save-thread-msg ,state ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (rem-thread-msg ,state)))))
