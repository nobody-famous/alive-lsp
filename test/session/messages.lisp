(defpackage :alive/test/session/messages
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)
                      (:init :alive/lsp/message/initialize)
                      (:utils :alive/test/utils)
                      (:run :alive/test/harness/run)
                      (:session :alive/session)))

(in-package :alive/test/session/messages)


(defclass init-msg-state (session::state)
    ())


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


(defmethod session::destroy ((obj init-msg-state))
    nil)


(defun init-msg ()
    (let ((state (make-instance 'init-msg-state
                                :logger (logger:create *standard-output* logger:*error*))))
        (run:test "Initialize Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))))))


(defun run-all ()
    (run:suite "Session Message Tests"
               (lambda ()
                   (init-msg))))
