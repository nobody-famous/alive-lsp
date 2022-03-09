(defpackage :alive/test/session/messages
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)
                      (:init :alive/lsp/message/initialize)
                      (:utils :alive/test/utils)
                      (:check :alive/test/harness/check)
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


(defclass completion-state (test-state)
    ())


(defclass top-form-state (test-state)
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


(defmethod session::send-msg ((obj init-msg-state) msg)
    (setf (send-called obj) T))


(defun init-msg ()
    (let ((state (create-state 'init-msg-state)))
        (run:test "Initialize Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))
                      (check:are-equal T (send-called state))))))


(defmethod session::get-input-stream ((obj load-file-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 0,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/loadFile\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"path\": \"test/files/compile/foo.lisp\",~A" utils:*end-line*)
                       (format str "    \"showStdout\": false~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (make-string-input-stream (utils:create-msg content))))


(defmethod session::send-msg ((obj load-file-state) msg)
    (setf (send-called obj) T))


(defun load-file-msg ()
    (let ((state (create-state 'load-file-state)))
        (run:test "Load File Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))
                      (check:are-equal t (send-called state))))))


(defmethod session::get-input-stream ((obj completion-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"textdocument/completion\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"textDocument\": {~A" utils:*end-line*)
                       (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                       (format str "    },~A" utils:*end-line*)
                       (format str "    \"position\": {~A" utils:*end-line*)
                       (format str "      \"line\": 3,~A" utils:*end-line*)
                       (format str "      \"character\": 11~A" utils:*end-line*)
                       (format str "    },~A" utils:*end-line*)
                       (format str "    \"context\": {~A" utils:*end-line*)
                       (format str "      \"triggerKind\": 1~A" utils:*end-line*)
                       (format str "    }~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (make-string-input-stream (utils:create-msg content))))


(defmethod session::send-msg ((obj completion-state) msg)
    (setf (send-called obj) T))


(defun completion-msg ()
    (let ((state (create-state 'completion-state)))
        (run:test "Completion Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))
                      (check:are-equal t (send-called state))))))


(defmethod session::get-input-stream ((obj top-form-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/topFormBounds\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"textDocument\": {~A" utils:*end-line*)
                       (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                       (format str "    },~A" utils:*end-line*)
                       (format str "    \"position\": {~A" utils:*end-line*)
                       (format str "      \"line\": 3,~A" utils:*end-line*)
                       (format str "      \"character\": 11~A" utils:*end-line*)
                       (format str "    }~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (make-string-input-stream (utils:create-msg content))))


(defmethod session::send-msg ((obj top-form-state) msg)
    (setf (send-called obj) T))


(defun top-form-msg ()
    (let ((state (create-state 'top-form-state)))
        (run:test "Top Form Message"
                  (lambda ()
                      (session::handle-msg state
                                           (session::read-message state))
                      (check:are-equal t (send-called state))))))


(defun run-all ()
    (run:suite "Session Message Tests"
               (lambda ()
                   (init-msg)
                   (load-file-msg)
                   (completion-msg)
                   (top-form-msg))))
