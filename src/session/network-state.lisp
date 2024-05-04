(defpackage :alive/session/network-state
    (:use :cl)
    (:export :create)
    (:local-nicknames (:packet :alive/lsp/packet)
                      (:state :alive/session/state)))

(in-package :alive/session/network-state)


(defclass network-state (state)
        ((conn :accessor conn
               :initform nil
               :initarg :conn)))


(declaim (ftype (function (usocket:stream-usocket) create)))
(defun create (conn)
    (make-instance 'network-state
        :conn conn
        :running nil
        :listeners nil
        :read-thread nil))


(defmethod destroy ((obj network-state))
    (when (conn obj)
          (usocket:socket-close (conn obj))
          (setf (conn obj) NIL)))


(defmethod get-input-stream ((obj network-state))
    (flexi-streams:make-flexi-stream
        (usocket:socket-stream (conn obj))))


(defmethod get-output-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod send-msg ((obj network-state) msg)
    (bt:with-recursive-lock-held ((state:lock obj))
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (usocket:socket-stream (conn obj)))
              (force-output (usocket:socket-stream (conn obj))))))
