(defpackage :alive/test/session/messages
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)
                      (:init :alive/lsp/message/initialize)
                      (:utils :alive/test/utils)
                      (:run :alive/test/harness/run)
                      (:session :alive/session)))

(in-package :alive/test/session/messages)


(defclass test-state (session::state)
    ((send-called :accessor send-called
                  :initform nil
                  :initarg :send-called)))


(defclass init-msg-state (test-state)
    ())


(defclass load-file-state (test-state)
    ())


(defun create-state (cls)
    (make-instance cls
                   :logger (logger:create *standard-output* logger:*error*)))


(defmethod session::get-input-stream ((obj init-msg-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 0,~A" utils:*end-line*)
                       (format str "  \"method\": \"initialize\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"clientInfo\": {~A" utils:*end-line*)
                       (format str "      \"name\": \"Visual Studio Code\",~A" utils:*end-line*)
                       (format str "      \"version\": \"1.62.3\"~A" utils:*end-line*)
                       (format str "    }~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (make-string-input-stream (utils:create-msg content))))


(defmethod session::send-msg ((obj test-state) msg)
    (setf (send-called obj) T))


(defun init-msg ()
    (let ((state (create-state 'init-msg-state)))
        (run:test "Initialize Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))
                      (unless (send-called state)
                              (error "Message send not called"))))))


(defmethod session::get-input-stream ((obj load-file-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 0,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/loadFile\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"path\": \"test/files/compile/foo.lisp\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (make-string-input-stream (utils:create-msg content))))


(defun load-file-msg ()
    (let ((state (create-state 'load-file-state)))
        (run:test "Load File Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))))))


(defun run-all ()
    (run:suite "Session Message Tests"
               (lambda ()
                   (init-msg)
                   (load-file-msg))))
