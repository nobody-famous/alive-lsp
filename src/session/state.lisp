(defpackage :alive/session/state
    (:use :cl)
    (:export :create
             :add-history
             :add-inspector
             :get-debugger
             :get-file-forms
             :get-file-text
             :get-file-tokens
             :get-history-item
             :get-inspector
             :get-log
             :get-sent-msg-callback
             :get-thread-msg
             :initialized
             :lock
             :next-inspector-id
             :next-send-id
             :next-thread-id
             :rem-inspector
             :remove-debugger
             :rem-thread-msg
             :running
             :set-debugger
             :set-file-forms
             :set-file-text
             :set-file-tokens
             :set-initialized
             :set-running
             :set-sent-msg-callback
             :with-thread-msg
             :state)
    (:local-nicknames (:deps :alive/deps)
                      (:logger :alive/logger)))

(in-package :alive/session/state)


(defstruct state
    (running nil :type boolean)
    (initialized nil :type boolean)

    (log nil :type (or null logger:logger))

    (files (make-hash-table :test 'equalp) :type hash-table)
    (forms (make-hash-table :test 'equalp) :type hash-table)
    (tokens (make-hash-table :test 'equalp) :type hash-table)
    (thread-msgs (make-hash-table :test 'equalp) :type hash-table)
    (sent-msg-callbacks (make-hash-table :test 'equalp) :type hash-table)
    (inspectors (make-hash-table :test 'equalp) :type hash-table)
    (debuggers (make-hash-table :test 'equalp) :type hash-table)

    (thread-name-id 1 :type integer)
    (send-msg-id 1 :type integer)
    (inspector-id 1 :type integer)

    (history (make-array 3) :type array)

    (lock (bt:make-recursive-lock) :type sb-thread:mutex))


(defun create (&key log)
    (make-state :log log))


(defun initialized (state)
    (state-initialized state))


(defun running (state)
    (state-running state))


(defun set-running (state value)
    (setf (state-running state) value))


(defmacro lock ((state mutex) &body body)
    `(progn (let ((,mutex (state-lock ,state)))
                (bt:with-recursive-lock-held (,mutex)
                    (progn ,@body)))))


(defun get-debugger (state id)
    (when (and state (numberp id))
          (gethash id (state-debuggers state))))


(defun remove-debugger (state id)
    (when (and state (numberp id))
          (remhash id (state-debuggers state))))


(defun set-debugger (state id frames)
    (when (and state
               (numberp id)
               (consp frames))
          (setf (gethash id (state-debuggers state)) frames)))


(defun get-log (state)
    (state-log state))


(defun get-sent-msg-callback (state id)
    (gethash id (state-sent-msg-callbacks state)))


(defun set-sent-msg-callback (state id cb)
    (setf (gethash id (state-sent-msg-callbacks state)) cb)
    nil)


(defun add-history (state item)
    (setf (elt (state-history state) 2) (elt (state-history state) 1))
    (setf (elt (state-history state) 1) (elt (state-history state) 0))
    (setf (elt (state-history state) 0) item))


(defun get-history-item (state index)
    (when (and (<= 0 index)
               (< index (length (state-history state))))
          (elt (state-history state) index)))


(defun set-initialized (state value)
    (setf (state-initialized state) value))


(defun set-file-text (state uri text)
    (setf (gethash uri (state-files state)) text))


(defun set-file-forms (state uri forms)
    (setf (gethash uri (state-forms state)) forms))


(defun set-file-tokens (state uri tokens)
    (setf (gethash uri (state-tokens state)) tokens))


(defun get-file-text (state uri)
    (gethash uri (state-files state)))


(defun get-file-forms (state uri)
    (gethash uri (state-forms state)))


(defun get-file-tokens (state uri)
    (gethash uri (state-tokens state)))


(defmacro next-id (state fn)
    `(progn (bt:with-recursive-lock-held ((state-lock ,state))
                (let ((id (,fn state)))
                    (incf (,fn state))
                    id))))


(defun next-send-id (state)
    (next-id state state-send-msg-id))


(defun next-inspector-id (state)
    (next-id state state-inspector-id))


(defun next-thread-id (state)
    (next-id state state-thread-name-id))


(defun add-inspector (state id inspector)
    (bt:with-recursive-lock-held ((state-lock state))
        (setf (gethash id (state-inspectors state))
            inspector)))


(defun rem-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (remhash id (state-inspectors state))))


(defun get-inspector (state id)
    (bt:with-recursive-lock-held ((state-lock state))
        (gethash id (state-inspectors state))))


(defun save-thread-msg (state deps id)
    (let* ((table (state-thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (deps:get-thread-id deps cur-thread)))

        (bt:with-recursive-lock-held ((state-lock state))
            (setf (gethash thread-id table) id))))


(defun get-thread-msg (state thread-id)
    (let ((table (state-thread-msgs state)))
        (bt:with-recursive-lock-held ((state-lock state))
            (gethash thread-id table))))


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
