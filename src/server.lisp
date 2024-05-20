(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:context :alive/context)
                      (:deps :alive/deps)
                      (:handlers :alive/session/handlers)
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


(declaim (ftype (function (cons) null) ignore-msg))
(defun ignore-msg (msg)
    (declare (ignore msg))
    nil)


(declaim (type (or null alive/session/handlers:list-of-handlers) *message-handlers*))
(defparameter *message-handlers* (list (cons "initialize" #'alive/session/handler/init:request)
                                       (cons "initialized" #'alive/session/handler/init:initialized)

                                       (cons "textDocument/completion" #'alive/session/handler/document:completion)
                                       (cons "textDocument/definition" #'alive/session/handler/document:definition)
                                       (cons "textDocument/didChange" #'alive/session/handler/document:did-change)
                                       (cons "textDocument/didClose" #'ignore-msg)
                                       (cons "textDocument/didOpen" #'alive/session/handler/document:did-open)
                                       (cons "textDocument/didSave" #'ignore-msg)
                                       (cons "textDocument/documentSymbol" #'alive/session/handler/document:doc-symbols)
                                       (cons "textDocument/hover" #'alive/session/handler/document:hover)
                                       (cons "textDocument/onTypeFormatting" #'alive/session/handler/document:on-type)
                                       (cons "textDocument/rangeFormatting" #'alive/session/handler/document:formatting)
                                       (cons "textDocument/selectionRange" #'alive/session/handler/document:selection)
                                       (cons "textDocument/semanticTokens/full" #'alive/session/handler/document:sem-tokens)

                                       (cons "$/setTrace" #'ignore-msg)
                                       (cons "$/cancelRequest" #'ignore-msg)

                                       (cons "$/alive/eval" #'alive/session/handler/eval:handle)
                                       (cons "$/alive/topFormBounds" #'alive/session/handler/form-bounds:top-form)
                                       (cons "$/alive/surroundingFormBounds" #'alive/session/handler/form-bounds:surrounding-form)
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
                                       #+n (cons "$/alive/compile" 'handle-compile)
                                       #+n (cons "$/alive/tryCompile" 'handle-try-compile)
                                       #+n (cons "$/alive/unexportSymbol" 'handle-unexport)))


(declaim (ftype (function () (or null cons)) read-msg))
(defun read-msg ()
    (parse:from-stream (context:get-input-stream)))


(declaim (ftype (function (T)) send-msg))
(defun send-msg (msg)
    (state:lock (mutex)
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (context:get-output-stream))
              (force-output (context:get-output-stream)))))


(declaim (ftype (function (stream) *) eval-fn))
(defun eval-fn (input)
    (eval (read input)))


(declaim (ftype (function () state:state) create-session-state))
(defun create-session-state ()
    (state:create))


(declaim (ftype (function () deps:deps) create-deps))
(defun create-deps ()
    (deps:create :msg-handler #'alive/session/message:handle
                 :send-msg #'send-msg
                 :read-msg #'read-msg
                 :eval-fn #'eval-fn))


(defun new-accept-conn ()
    (let* ((conn (usocket:socket-accept (socket *server*) :element-type '(unsigned-byte 8))))
        (context:with-context (:input-stream (flexi-streams:make-flexi-stream (usocket:socket-stream conn))
                                             :output-stream (usocket:socket-stream conn)
                                             :destroy-fn (lambda ()
                                                             (usocket:socket-close conn)))
            (alive/deps:with-deps (create-deps)
                (handlers:with-handlers *message-handlers*
                    (session::new-start))))))


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
          #+n (new-accept-conn)
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
