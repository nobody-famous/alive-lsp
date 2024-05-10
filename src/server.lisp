(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:context :alive/context)
                      (:deps :alive/session/deps)
                      (:logger :alive/logger)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:session :alive/session)
                      (:state :alive/session/state)))

(in-package :alive/server)


(defvar *default-port* 0)
(defparameter *server* nil)

(defclass lsp-server ()
        ((running :accessor running
                  :initform nil
                  :initarg :running)
         (lock :accessor lock
               :initform (bt:make-recursive-lock)
               :initarg :lock)
         (sessions :accessor sessions
                   :initform nil
                   :initarg :sessions)
         (socket :accessor socket
                 :initform nil
                 :initarg :socket)))


(declaim (type (or null alive/session/handlers:list-of-handlers) *default-handlers*))
(defparameter *default-handlers* (list (cons "initialize" #'alive/session/handler/init:request)
                                       (cons "initialized" #'alive/session/handler/init:initialized)

                                       (cons "textDocument/completion" #'alive/session/handler/document:completion)
                                       #+n (cons "textDocument/definition" 'handle-definition)
                                       #+n (cons "textDocument/didChange" 'handle-did-change)
                                       #+n (cons "textDocument/didClose" 'handle-did-change)
                                       #+n (cons "textDocument/didOpen" 'handle-did-open)
                                       #+n (cons "textDocument/didSave" 'ignore-msg)
                                       #+n (cons "textDocument/documentSymbol" 'handle-doc-symbols)
                                       #+n (cons "textDocument/hover" 'handle-hover)
                                       #+n (cons "textDocument/onTypeFormatting" 'handle-on-type)
                                       #+n (cons "textDocument/rangeFormatting" 'handle-formatting)
                                       #+n (cons "textDocument/selectionRange" 'handle-selection)
                                       #+n (cons "textDocument/semanticTokens/full" 'handle-sem-tokens)

                                       #+n (cons "$/setTrace" 'ignore-msg)
                                       #+n (cons "$/cancelRequest" 'ignore-msg)

                                       #+n (cons "$/alive/eval" 'handle-eval)
                                       #+n (cons "$/alive/getPackageForPosition" 'handle-get-pkg)
                                       #+n (cons "$/alive/inspect" 'handle-inspect)
                                       #+n (cons "$/alive/inspectClose" 'handle-inspect-close)
                                       #+n (cons "$/alive/inspectEval" 'handle-inspect-eval)
                                       #+n (cons "$/alive/inspectMacro" 'handle-inspect-macro)
                                       #+n (cons "$/alive/inspectRefresh" 'handle-inspect-refresh)
                                       #+n (cons "$/alive/inspectSymbol" 'handle-inspect-sym)
                                       #+n (cons "$/alive/killThread" 'handle-kill-thread)
                                       #+n (cons "$/alive/listAsdfSystems" 'handle-list-asdf)
                                       #+n (cons "$/alive/listPackages" 'handle-list-pkgs)
                                       #+n (cons "$/alive/listThreads" 'handle-list-threads)
                                       #+n (cons "$/alive/loadFile" 'handle-load-file)
                                       #+n (cons "$/alive/loadAsdfSystem" 'handle-load-asdf)
                                       #+n (cons "$/alive/macroexpand" 'handle-macroexpand)
                                       #+n (cons "$/alive/macroexpand1" 'handle-macroexpand-1)
                                       #+n (cons "$/alive/removePackage" 'handle-remove-pkg)
                                       #+n (cons "$/alive/symbol" 'handle-symbol)
                                       #+n (cons "$/alive/surroundingFormBounds" 'handle-surrounding-form)
                                       #+n (cons "$/alive/topFormBounds" 'handle-top-form)
                                       #+n (cons "$/alive/compile" 'handle-compile)
                                       #+n (cons "$/alive/tryCompile" 'handle-try-compile)
                                       #+n (cons "$/alive/unexportSymbol" 'handle-unexport)))


(declaim (ftype (function () (or null cons)) read-msg))
(defun read-msg ()
    (parse:from-stream (context:get-input-stream)))


(declaim (ftype (function (T)) send-msg))
(defun send-msg (msg)
    (bt:with-recursive-lock-held ((state:lock))
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (context:get-output-stream))
              (force-output (context:get-output-stream)))))


(declaim (ftype (function () state:state) create-session-state))
(defun create-session-state ()
    (state:create))


(declaim (ftype (function () deps:deps) create-deps))
(defun create-deps ()
    (deps:create :msg-handler #'alive/session/message:handle
                 :send-msg #'send-msg
                 :read-msg #'read-msg))


(defun accept-conn ()
    (let* ((conn (usocket:socket-accept (socket *server*) :element-type '(unsigned-byte 8)))
           (session (session:create :conn conn)))

        (session:add-listener session
                              (make-instance 'session:listener
                                  :on-done (lambda ()
                                               (usocket:socket-close conn)
                                               (setf (sessions *server*)
                                                   (remove session (sessions *server*))))))
        (session:start session)

        (push session (sessions *server*))))


(defun wait-for-conn ()
    (usocket:wait-for-input (socket *server*))

    (when (and (running *server*)
               (usocket::state (socket *server*)))
          (accept-conn)))


(defun wake-up-accept ()
    (ignore-errors
        (let ((conn (usocket:socket-connect "127.0.0.1" (usocket:get-local-port (socket *server*)))))
            (usocket:socket-close conn))))


(defun stop-server ()
    (bt:with-recursive-lock-held ((lock *server*))
        (setf (running *server*) nil)

        (loop :for session :in (sessions *server*) :do
                  (session:stop session))

        (setf (sessions *server*) nil)

        (when (socket *server*)
              (wake-up-accept)
              (usocket:socket-close (socket *server*))
              (setf (socket *server*) nil))))


(defun listen-for-conns (port)
    (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (format T "[~A][STARTING] Started on port ~A~%" (alive/utils:get-timestamp) (usocket:get-local-port socket))

        (unwind-protect
                (progn (setf (socket *server*) socket)
                       (setf (running *server*) T)

                       (loop :while (running *server*)
                             :do (wait-for-conn)))
            (stop-server))))


(defun start-server (port)
    (let ((stdout *standard-output*)
          (server *server*)
          (logger logger:*logger*))
        (bt:make-thread (lambda ()
                            (let ((*server* server)
                                  (logger:*logger* logger))
                                (let ((*standard-output* stdout))
                                    (listen-for-conns port))))
                        :name "Alive LSP Server")))


(defun stop ()
    (logger:info-msg "Stop server")

    (stop-server)

    (setf *server* nil))


(defun start (&key (port *default-port*))
    (logger:with-logging (logger:create *standard-output* logger:*info*)
        (let ((*server* (make-instance 'lsp-server)))
            (start-server port))))
