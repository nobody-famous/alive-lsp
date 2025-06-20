(defpackage :alive/session/state
    (:use :cl)
    (:export :create
             :create-listener
             :listener
             :add-history
             :add-inspector
             :add-listener
             :get-file-text
             :get-history-item
             :get-inspector
             :get-log
             :get-sent-msg-callback
             :get-thread-msg
             :initialized
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
             :with-thread-msg
             :state)
    (:local-nicknames (:deps :alive/deps)
                      (:logger :alive/logger)))

(in-package :alive/session/state)


(defstruct listener
    (on-done nil :type (or null function)))


(declaim (ftype (function (function) listener) create-listener))
(defun create-listener (done-fn)
    (make-listener :on-done done-fn))


(defstruct state
    (running nil :type boolean)
    (initialized nil :type boolean)

    (log nil :type (or null logger:logger))

    (files (make-hash-table :test 'equalp) :type hash-table)
    (thread-msgs (make-hash-table :test 'equalp) :type hash-table)
    (sent-msg-callbacks (make-hash-table :test 'equalp) :type hash-table)
    (inspectors (make-hash-table :test 'equalp) :type hash-table)

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (listeners nil :type (or null cons))

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex))


(declaim (ftype (function (&key (:log (or null logger:logger))) state) create))
(defun create (&key log)
    (make-state :log log))


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


(defmacro lock ((state mutex) &body body)
    `(progn (let ((,mutex (state-lock ,state)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(declaim (ftype (function (state) (or null logger:logger)) get-log))
(defun get-log (state)
    (state-log state))


(declaim (ftype (function (state fixnum) (or null function)) get-sent-msg-callback))
(defun get-sent-msg-callback (state id)
    (gethash id (state-sent-msg-callbacks state)))


(declaim (ftype (function (state fixnum (function (cons) (or null hash-table))) null) set-sent-msg-callback))
(defun set-sent-msg-callback (state id cb)
    (setf (gethash id (state-sent-msg-callbacks state)) cb)
    nil)


(declaim (ftype (function (state T)) add-history))
(defun add-history (state item)
    (setf (elt (state-history state) 2) (elt (state-history state) 1))
    (setf (elt (state-history state) 1) (elt (state-history state) 0))
    (setf (elt (state-history state) 0) item))


(declaim (ftype (function (state integer) T) get-history-item))
(defun get-history-item (state index)
    (when (and (<= 0 index)
               (< index (length (state-history state))))
          (elt (state-history state) index)))


(declaim (ftype (function (state listener)) add-listener))
(defun add-listener (state to-add)
    (push to-add (state-listeners state)))


(declaim (ftype (function (state boolean)) set-initialized))
(defun set-initialized (state value)
    (setf (state-initialized state) value))


(declaim (ftype (function (state string string)) set-file-text))
(defun set-file-text (state uri text)
    (alive/logger:info-msg (get-log state) "***** SET TEXT ~A" uri)
    (setf (gethash uri (state-files state)) text))


(declaim (ftype (function (state string) (or null string)) get-file-text))
(defun get-file-text (state uri)
    (gethash uri (state-files state)))


(defmacro next-id (state fn)
    `(progn (bt:with-recursive-lock-held ((state-lock ,state))
                (let ((id (,fn state)))
                    (incf (,fn state))
                    id))))


(declaim (ftype (function (state) integer) next-send-id))
(defun next-send-id (state)
    (next-id state state-send-msg-id))


(declaim (ftype (function (state) integer) next-inspector-id))
(defun next-inspector-id (state)
    (next-id state state-inspector-id))


(declaim (ftype (function (state) integer) next-thread-id))
(defun next-thread-id (state)
    (next-id state state-thread-name-id))


(declaim (ftype (function (state integer alive/inspector:inspector)) add-inspector))
(defun add-inspector (state id inspector)
    (bt:with-recursive-lock-held ((state-lock state))
        (setf (gethash id (state-inspectors state))
            inspector)))


(declaim (ftype (function (state integer)) rem-inspector))
(defun rem-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (remhash id (state-inspectors state))))


(declaim (ftype (function (state integer) (or null alive/inspector:inspector)) get-inspector))
(defun get-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (gethash id (state-inspectors state))))


(declaim (ftype (function (state deps:dependencies T)) save-thread-msg))
(defun save-thread-msg (state deps id)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function (state T) (or null integer)) get-thread-msg))
(defun get-thread-msg (state thread-id)
    (let ((table (state-thread-msgs state)))
        (bt:with-recursive-lock-held ((state-lock state))
            (gethash thread-id table))))


(declaim (ftype (function (state deps:dependencies) boolean) rem-thread-msg))
(defun rem-thread-msg (state deps)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (remhash thread-id table))))


(defmacro with-thread-msg ((state deps id) &body body)
    `(progn (when ,id (save-thread-msg ,state ,deps ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (rem-thread-msg ,state ,deps)))))
