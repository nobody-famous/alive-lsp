(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:logger :alive/logger)
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


#+n (declaim (type (or null alive/session/handlers:list-of-handlers) *default-handlers*))
#+n (defparameter *default-handlers* (list (cons "initialize" 'handle-init)
                                           (cons "initialized" 'handle-initialized)

                                           (cons "textDocument/completion" 'handle-completion)
                                           (cons "textDocument/definition" 'handle-definition)
                                           (cons "textDocument/didChange" 'handle-did-change)
                                           (cons "textDocument/didClose" 'handle-did-change)
                                           (cons "textDocument/didOpen" 'handle-did-open)
                                           (cons "textDocument/didSave" 'ignore-msg)
                                           (cons "textDocument/documentSymbol" 'handle-doc-symbols)
                                           (cons "textDocument/hover" 'handle-hover)
                                           (cons "textDocument/onTypeFormatting" 'handle-on-type)
                                           (cons "textDocument/rangeFormatting" 'handle-formatting)
                                           (cons "textDocument/selectionRange" 'handle-selection)
                                           (cons "textDocument/semanticTokens/full" 'handle-sem-tokens)

                                           (cons "$/setTrace" 'ignore-msg)
                                           (cons "$/cancelRequest" 'ignore-msg)

                                           (cons "$/alive/eval" 'handle-eval)
                                           (cons "$/alive/getPackageForPosition" 'handle-get-pkg)
                                           (cons "$/alive/inspect" 'handle-inspect)
                                           (cons "$/alive/inspectClose" 'handle-inspect-close)
                                           (cons "$/alive/inspectEval" 'handle-inspect-eval)
                                           (cons "$/alive/inspectMacro" 'handle-inspect-macro)
                                           (cons "$/alive/inspectRefresh" 'handle-inspect-refresh)
                                           (cons "$/alive/inspectSymbol" 'handle-inspect-sym)
                                           (cons "$/alive/killThread" 'handle-kill-thread)
                                           (cons "$/alive/listAsdfSystems" 'handle-list-asdf)
                                           (cons "$/alive/listPackages" 'handle-list-pkgs)
                                           (cons "$/alive/listThreads" 'handle-list-threads)
                                           (cons "$/alive/loadFile" 'handle-load-file)
                                           (cons "$/alive/loadAsdfSystem" 'handle-load-asdf)
                                           (cons "$/alive/macroexpand" 'handle-macroexpand)
                                           (cons "$/alive/macroexpand1" 'handle-macroexpand-1)
                                           (cons "$/alive/removePackage" 'handle-remove-pkg)
                                           (cons "$/alive/symbol" 'handle-symbol)
                                           (cons "$/alive/surroundingFormBounds" 'handle-surrounding-form)
                                           (cons "$/alive/topFormBounds" 'handle-top-form)
                                           (cons "$/alive/compile" 'handle-compile)
                                           (cons "$/alive/tryCompile" 'handle-try-compile)
                                           (cons "$/alive/unexportSymbol" 'handle-unexport)))


(declaim (ftype (function () state:state) create-session-state))
(defun create-session-state ()
    (state:create :msg-handler #'alive/session/message:handle
                  :send-msg #'alive/session/io:send-msg
                  :read-msg #'alive/session/io:read-msg))


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
