(defpackage :alive/session/state
    (:use :cl)
    (:export :add-history
             :add-inspector
             :add-listener
             :create
             :create-listener
             :get-file-text
             :get-history-item
             :get-inspector
             :get-sent-msg-callback
             :get-thread-msg
             :initialized
             :listener
             :listeners
             :lock
             :new-add-listener
             :new-get-file-text
             :new-get-history-item
             :new-get-sent-msg-callback
             :new-lock
             :new-next-send-id
             :new-next-thread-id
             :new-rem-thread-msg
             :new-running
             :new-set-initialized
             :new-set-running
             :new-set-sent-msg-callback
             :new-with-thread-msg
             :next-inspector-id
             :next-send-id
             :new-set-file-text
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
    (:local-nicknames (:deps :alive/deps)))

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

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (listeners nil :type (or null cons))

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex))


(declaim (ftype (function () state) create))
(defun create ()
    (make-state))


(declaim (ftype (function () (or null cons)) listeners))
(defun listeners ()
    (unless *state* (error "listenersState not set"))
    (state-listeners *state*))


(declaim (ftype (function () boolean) initialized))
(defun initialized ()
    (unless *state* (error "initialized State not set"))
    (state-initialized *state*))


(declaim (ftype (function () boolean) running))
(defun running ()
    (unless *state* (error "running State not set"))
    (state-running *state*))


(declaim (ftype (function (state) boolean) new-running))
(defun new-running (state)
    (state-running state))


(declaim (ftype (function (boolean)) set-running))
(defun set-running (value)
    (unless *state* (error "set-running State not set"))
    (setf (state-running *state*) value))


(declaim (ftype (function (state boolean)) new-set-running))
(defun new-set-running (state value)
    (setf (state-running state) value))


(defmacro lock ((mutex) &body body)
    `(progn (unless *state* (error "lock State not set"))
            (let ((,mutex (state-lock *state*)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(defmacro new-lock ((state mutex) &body body)
    `(progn (let ((,mutex (state-lock ,state)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(declaim (ftype (function (fixnum) (or null function)) get-sent-msg-callback))
(defun get-sent-msg-callback (id)
    (unless *state* (error "get-sent-msg-callback State not set"))
    (gethash id (state-sent-msg-callbacks *state*)))


(declaim (ftype (function (state fixnum) (or null function)) new-get-sent-msg-callback))
(defun new-get-sent-msg-callback (state id)
    (gethash id (state-sent-msg-callbacks state)))


(declaim (ftype (function (fixnum (function (cons) (or null hash-table))) null) set-sent-msg-callback))
(defun set-sent-msg-callback (id cb)
    (unless *state* (error "set-sent-msg-callback State not set"))
    (setf (gethash id (state-sent-msg-callbacks *state*)) cb)
    nil)


(declaim (ftype (function (state fixnum (function (cons) (or null hash-table))) null) new-set-sent-msg-callback))
(defun new-set-sent-msg-callback (state id cb)
    (setf (gethash id (state-sent-msg-callbacks state)) cb)
    nil)


(declaim (ftype (function (T)) add-history))
(defun add-history (item)
    (unless *state* (error "add-history State not set"))
    (setf (elt (state-history *state*) 2) (elt (state-history *state*) 1))
    (setf (elt (state-history *state*) 1) (elt (state-history *state*) 0))
    (setf (elt (state-history *state*) 0) item))


(declaim (ftype (function (integer) T) get-history-item))
(defun get-history-item (index)
    (unless *state* (error "get-history-item State not set"))
    (when (and (<= 0 index)
               (< index (length (state-history *state*))))
          (elt (state-history *state*) index)))


(declaim (ftype (function (state integer) T) new-get-history-item))
(defun new-get-history-item (state index)
    (when (and (<= 0 index)
               (< index (length (state-history state))))
          (elt (state-history state) index)))


(declaim (ftype (function (listener)) add-listener))
(defun add-listener (to-add)
    (unless *state* (error "add-listener State not set"))
    (push to-add (state-listeners *state*)))


(declaim (ftype (function (state listener)) new-add-listener))
(defun new-add-listener (state to-add)
    (push to-add (state-listeners state)))


(declaim (ftype (function (boolean)) set-initialized))
(defun set-initialized (value)
    (unless *state* (error "set-initialized State not set"))
    (setf (state-initialized *state*) value))


(declaim (ftype (function (state boolean)) new-set-initialized))
(defun new-set-initialized (state value)
    (setf (state-initialized state) value))


(declaim (ftype (function (string string)) set-file-text))
(defun set-file-text (uri text)
    (unless *state* (error "set-file-text State not set"))
    (setf (gethash uri (state-files *state*)) text))


(declaim (ftype (function (state string string)) new-set-file-text))
(defun new-set-file-text (state uri text)
    (setf (gethash uri (state-files state)) text))


(declaim (ftype (function (string) (or null string)) get-file-text))
(defun get-file-text (uri)
    (unless *state* (error "get-file-text State not set"))
    (gethash uri (state-files *state*)))


(declaim (ftype (function (state string) (or null string)) new-get-file-text))
(defun new-get-file-text (state uri)
    (gethash uri (state-files state)))


(defmacro next-id (fn)
    `(progn (unless *state* (error "next-id State not set"))
            (bt:with-recursive-lock-held ((state-lock *state*))
                (let ((id (,fn *state*)))
                    (incf (,fn *state*))
                    id))))


(defmacro new-next-id (state fn)
    `(progn (bt:with-recursive-lock-held ((state-lock ,state))
                (let ((id (,fn state)))
                    (incf (,fn state))
                    id))))


(declaim (ftype (function () integer) next-send-id))
(defun next-send-id ()
    (alive/logger:info-msg "***** OLD NEXT SEND ID")
    (next-id state-send-msg-id))


(declaim (ftype (function (state) integer) new-next-send-id))
(defun new-next-send-id (state)
    (new-next-id state state-send-msg-id))


(declaim (ftype (function () integer) next-inspector-id))
(defun next-inspector-id ()
    (next-id state-inspector-id))


(declaim (ftype (function () integer) next-thread-id))
(defun next-thread-id ()
    (alive/logger:info-msg "***** OLD NEXT THREAD ID")
    (next-id state-thread-name-id))


(declaim (ftype (function (state) integer) new-next-thread-id))
(defun new-next-thread-id (state)
    (new-next-id state state-thread-name-id))


(declaim (ftype (function (integer alive/inspector:inspector)) add-inspector))
(defun add-inspector (id inspector)
    (unless *state* (error "add-inspector State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (setf (gethash id (state-inspectors *state*))
            inspector)))


(declaim (ftype (function (integer)) rem-inspector))
(defun rem-inspector (id)
    (unless *state* (error "rem-inspector State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (remhash id (state-inspectors *state*))))


(declaim (ftype (function (integer) (or null alive/inspector:inspector)) get-inspector))
(defun get-inspector (id)
    (unless *state* (error "get-inspector State not set"))
    (bt:with-recursive-lock-held ((state-lock *state*))
        (gethash id (state-inspectors *state*))))


(declaim (ftype (function (T)) save-thread-msg))
(defun save-thread-msg (id)
    (let* ((table (state-thread-msgs *state*))
           (cur-thread (bt:current-thread))
           (thread-id (deps:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock *state*))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function (deps:dependencies state T)) new-save-thread-msg))
(defun new-save-thread-msg (deps state id)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:new-get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (setf (gethash thread-id table) id))))


(declaim (ftype (function (T) (or null integer)) get-thread-msg))
(defun get-thread-msg (thread-id)
    (let ((table (state-thread-msgs *state*)))

        (bt:with-recursive-lock-held ((state-lock *state*))
            (gethash thread-id table))))


(declaim (ftype (function () boolean) rem-thread-msg))
(defun rem-thread-msg ()
    (let* ((table (state-thread-msgs *state*))
           (cur-thread (bt:current-thread))
           (thread-id (deps:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((state-lock *state*))
            (remhash thread-id table))))


(declaim (ftype (function (deps:dependencies state) boolean) new-rem-thread-msg))
(defun new-rem-thread-msg (deps state)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:new-get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (remhash thread-id table))))


(defmacro with-thread-msg ((id) &body body)
    `(progn (unless *state* (error "with-thread-msg State not set"))
            (when ,id (save-thread-msg ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (rem-thread-msg)))))


(defmacro new-with-thread-msg ((deps state id) &body body)
    `(progn (when ,id (new-save-thread-msg ,deps ,state ,id))
            (unwind-protect
                    (progn ,@body)
                (when ,id (new-rem-thread-msg ,deps ,state)))))


(defmacro with-state (state &body body)
    `(let ((*state* ,state))
         (progn ,@body)))
