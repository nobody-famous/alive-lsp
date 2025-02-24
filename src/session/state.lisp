(defpackage :alive/session/state
    (:use :cl)
    (:export :create
             :create-listener
             :new-add-history
             :new-add-inspector
             :new-add-listener
             :new-get-file-text
             :new-get-history-item
             :new-get-inspector
             :new-get-sent-msg-callback
             :new-get-thread-msg
             :new-initialized
             :new-listeners
             :new-lock
             :new-next-inspector-id
             :new-next-send-id
             :new-next-thread-id
             :new-rem-inspector
             :new-rem-thread-msg
             :new-running
             :new-set-file-text
             :new-set-initialized
             :new-set-running
             :new-set-sent-msg-callback
             :new-with-thread-msg
             :state)
    (:local-nicknames (:deps :alive/deps)))

(in-package :alive/session/state)


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

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (listeners nil :type (or null cons))

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex))


(declaim (ftype (function () state) create))
(defun create ()
    (make-state))


(declaim (ftype (function (state) (or null cons)) new-listeners))
(defun new-listeners (state)
    (state-listeners state))


(declaim (ftype (function (state) boolean) new-initialized))
(defun new-initialized (state)
    (state-initialized state))


(declaim (ftype (function (state) boolean) new-running))
(defun new-running (state)
    (state-running state))


(declaim (ftype (function (state boolean)) new-set-running))
(defun new-set-running (state value)
    (setf (state-running state) value))


(defmacro new-lock ((state mutex) &body body)
    `(progn (let ((,mutex (state-lock ,state)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(declaim (ftype (function (state fixnum) (or null function)) new-get-sent-msg-callback))
(defun new-get-sent-msg-callback (state id)
    (gethash id (state-sent-msg-callbacks state)))


(declaim (ftype (function (state fixnum (function (cons) (or null hash-table))) null) new-set-sent-msg-callback))
(defun new-set-sent-msg-callback (state id cb)
    (setf (gethash id (state-sent-msg-callbacks state)) cb)
    nil)


(declaim (ftype (function (state T)) new-add-history))
(defun new-add-history (state item)
    (setf (elt (state-history state) 2) (elt (state-history state) 1))
    (setf (elt (state-history state) 1) (elt (state-history state) 0))
    (setf (elt (state-history state) 0) item))


(declaim (ftype (function (state integer) T) new-get-history-item))
(defun new-get-history-item (state index)
    (when (and (<= 0 index)
               (< index (length (state-history state))))
          (elt (state-history state) index)))


(declaim (ftype (function (state listener)) new-add-listener))
(defun new-add-listener (state to-add)
    (push to-add (state-listeners state)))


(declaim (ftype (function (state boolean)) new-set-initialized))
(defun new-set-initialized (state value)
    (setf (state-initialized state) value))


(declaim (ftype (function (state string string)) new-set-file-text))
(defun new-set-file-text (state uri text)
    (setf (gethash uri (state-files state)) text))


(declaim (ftype (function (state string) (or null string)) new-get-file-text))
(defun new-get-file-text (state uri)
    (gethash uri (state-files state)))


(defmacro new-next-id (state fn)
    `(progn (bt:with-recursive-lock-held ((state-lock ,state))
                (let ((id (,fn state)))
                    (incf (,fn state))
                    id))))


(declaim (ftype (function (state) integer) new-next-send-id))
(defun new-next-send-id (state)
    (new-next-id state state-send-msg-id))


(declaim (ftype (function (state) integer) new-next-inspector-id))
(defun new-next-inspector-id (state)
    (new-next-id state state-inspector-id))


(declaim (ftype (function (state) integer) new-next-thread-id))
(defun new-next-thread-id (state)
    (new-next-id state state-thread-name-id))


(declaim (ftype (function (state integer alive/inspector:inspector)) new-add-inspector))
(defun new-add-inspector (state id inspector)
    (bt:with-recursive-lock-held ((state-lock state))
        (setf (gethash id (state-inspectors state))
            inspector)))


(declaim (ftype (function (state integer)) new-rem-inspector))
(defun new-rem-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (remhash id (state-inspectors state))))


(declaim (ftype (function (state integer) (or null alive/inspector:inspector)) new-get-inspector))
(defun new-get-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (gethash id (state-inspectors state))))


(declaim (ftype (function (deps:dependencies state T)) new-save-thread-msg))
(defun new-save-thread-msg (deps state id)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:new-get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function (state T) (or null integer)) new-get-thread-msg))
(defun new-get-thread-msg (state thread-id)
    (let ((table (state-thread-msgs state)))
        (bt:with-recursive-lock-held ((state-lock state))
            (gethash thread-id table))))


(declaim (ftype (function (deps:dependencies state) boolean) new-rem-thread-msg))
(defun new-rem-thread-msg (deps state)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:new-get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (remhash thread-id table))))


(defmacro new-with-thread-msg ((deps state id) &body body)
    `(progn (when ,id (new-save-thread-msg ,deps ,state ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (new-rem-thread-msg ,deps ,state)))))
