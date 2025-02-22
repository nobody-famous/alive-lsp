(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:context :alive/context)
                      (:deps :alive/deps)
                      (:handlers :alive/session/handlers)
                      (:logger :alive/logger)
                      (:packet :alive/lsp/packet)
                      (:session :alive/session)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

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
         (socket :accessor socket
                 :initform nil
                 :initarg :socket)))


(declaim (ftype (function (cons) null) ignore-msg))
(defun ignore-msg (msg)
    (declare (ignore msg))
    nil)


(declaim (type (or null alive/session/handlers:list-of-handlers) *message-handlers*))
(defparameter *message-handlers*
              (list (cons "initialize" (lambda (msg) (alive/session/handler/init:request msg)))
                    (cons "initialized" (lambda (msg) (alive/session/handler/init:initialized msg)))

                    (cons "textDocument/completion" (lambda (msg) (alive/session/handler/document:completion msg)))
                    (cons "textDocument/definition" (lambda (msg) (alive/session/handler/document:definition msg)))
                    (cons "textDocument/didChange" (lambda (msg) (alive/session/handler/document:did-change msg)))
                    (cons "textDocument/didClose" (lambda (msg) (ignore-msg msg)))
                    (cons "textDocument/didOpen" (lambda (msg) (alive/session/handler/document:did-open msg)))
                    (cons "textDocument/didSave" (lambda (msg) (ignore-msg msg)))
                    (cons "textDocument/documentSymbol" (lambda (msg) (alive/session/handler/document:doc-symbols msg)))
                    (cons "textDocument/hover" (lambda (msg) (alive/session/handler/document:hover msg)))
                    (cons "textDocument/onTypeFormatting" (lambda (msg) (alive/session/handler/document:on-type msg)))
                    (cons "textDocument/rangeFormatting" (lambda (msg) (alive/session/handler/document:formatting msg)))
                    (cons "textDocument/selectionRange" (lambda (msg) (alive/session/handler/document:selection msg)))
                    (cons "textDocument/semanticTokens/full" (lambda (msg) (alive/session/handler/document:sem-tokens msg)))

                    (cons "$/setTrace" (lambda (msg) (ignore-msg msg)))
                    (cons "$/cancelRequest" (lambda (msg) (ignore-msg msg)))

                    (cons "$/alive/eval" (lambda (msg)
                                             (threads:run-in-thread (or (cdr (assoc :method msg)) "eval")
                                                                    (cdr (assoc :id msg))
                                                                    (lambda ()
                                                                        (alive/session/handler/eval:handle msg)))))
                    (cons "$/alive/topFormBounds" (lambda (msg) (alive/session/handler/form-bounds:top-form msg)))
                    (cons "$/alive/surroundingFormBounds" (lambda (msg) (alive/session/handler/form-bounds:surrounding-form msg)))

                    (cons "$/alive/getPackageForPosition" (lambda (msg) (alive/session/handler/packages:for-position msg)))
                    (cons "$/alive/listPackages" (lambda (msg) (alive/session/handler/packages:list-all msg)))
                    (cons "$/alive/removePackage" (lambda (msg) (alive/session/handler/packages:remove-pkg msg)))

                    (cons "$/alive/listThreads" (lambda (msg) (alive/session/handler/threads:list-all msg)))
                    (cons "$/alive/killThread" (lambda (msg) (alive/session/handler/threads:kill msg)))

                    (cons "$/alive/listAsdfSystems" (lambda (msg) (alive/session/handler/asdf:list-all msg)))
                    (cons "$/alive/loadAsdfSystem" (lambda (msg)
                                                       (threads:run-in-thread (or (cdr (assoc :method msg)) "ASDF")
                                                                              (cdr (assoc :id msg))
                                                                              (lambda ()
                                                                                  (alive/session/handler/asdf:load-system msg)))))

                    (cons "$/alive/tryCompile" (lambda (msg)
                                                   (spawn:new-thread "Try Compile"
                                                       (alive/session/handler/compile:try msg))))
                    (cons "$/alive/compile" (lambda (msg)
                                                (threads:run-in-thread (or (cdr (assoc :method msg)) "Compile")
                                                                       (cdr (assoc :id msg))
                                                                       (lambda ()
                                                                           (alive/session/handler/compile:file msg)))))
                    (cons "$/alive/loadFile" (lambda (msg)
                                                 (threads:run-in-thread (or (cdr (assoc :method msg)) "Load File")
                                                                        (cdr (assoc :id msg))
                                                                        (lambda ()
                                                                            (alive/session/handler/compile:load-file msg)))))

                    (cons "$/alive/symbol" (lambda (msg) (alive/session/handler/symbol:for-pos msg)))
                    (cons "$/alive/unexportSymbol" (lambda (msg) (alive/session/handler/symbol:do-unexport msg)))

                    (cons "$/alive/macroexpand" (lambda (msg) (alive/session/handler/macro:expand msg)))
                    (cons "$/alive/macroexpand1" (lambda (msg) (alive/session/handler/macro:expand-1 msg)))

                    (cons "$/alive/inspect" (lambda (msg)
                                                (threads:run-in-thread (or (cdr (assoc :method msg)) "Inspect")
                                                                       (cdr (assoc :id msg))
                                                                       (lambda ()
                                                                           (alive/session/handler/inspect:do-inspect msg)))))
                    (cons "$/alive/inspectRefresh" (lambda (msg) (alive/session/handler/inspect:refresh msg)))
                    (cons "$/alive/inspectClose" (lambda (msg) (alive/session/handler/inspect:do-close msg)))
                    (cons "$/alive/inspectSymbol" (lambda (msg)
                                                      (threads:run-in-thread (or (cdr (assoc :method msg)) "Inspect")
                                                                             (cdr (assoc :id msg))
                                                                             (lambda ()
                                                                                 (alive/session/handler/inspect:do-symbol msg)))))
                    (cons "$/alive/inspectEval" (lambda (msg) (threads:run-in-thread (or (cdr (assoc :method msg)) "Inspect")
                                                                                     (cdr (assoc :id msg))
                                                                                     (lambda ()
                                                                                         (alive/session/handler/inspect:do-inspect-eval msg)))))
                    (cons "$/alive/inspectMacro" (lambda (msg) (alive/session/handler/inspect:macro msg)))))


(declaim (type (or null alive/session/handlers:list-of-handlers) *new-message-handlers*))
(defparameter *new-message-handlers*
              (list (cons "initialize" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/init:request msg)))
                    (cons "initialized" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/init:new-initialized state msg)))

                    (cons "textDocument/completion" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-completion state msg)))
                    (cons "textDocument/definition" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-definition state msg)))
                    (cons "textDocument/didChange" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-did-change state msg)))
                    (cons "textDocument/didClose" (lambda (deps state msg) (declare (ignore deps state)) (ignore-msg msg)))
                    (cons "textDocument/didOpen" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-did-open state msg)))
                    (cons "textDocument/didSave" (lambda (deps state msg) (declare (ignore deps state)) (ignore-msg msg)))
                    (cons "textDocument/documentSymbol" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-doc-symbols state msg)))
                    (cons "textDocument/hover" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-hover state msg)))
                    (cons "textDocument/onTypeFormatting" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-on-type state msg)))
                    (cons "textDocument/rangeFormatting" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-formatting state msg)))
                    (cons "textDocument/selectionRange" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-selection state msg)))
                    (cons "textDocument/semanticTokens/full" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/document:new-sem-tokens state msg)))

                    (cons "$/setTrace" (lambda (deps state msg) (declare (ignore deps state)) (ignore-msg msg)))
                    (cons "$/cancelRequest" (lambda (deps state msg) (declare (ignore deps state)) (ignore-msg msg)))

                    (cons "$/alive/eval" (lambda (deps state msg)
                                             (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "eval")
                                                                        (cdr (assoc :id msg))
                                                                        (lambda ()
                                                                            (alive/session/handler/eval:new-handle deps state msg)))))
                    (cons "$/alive/topFormBounds" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/form-bounds:top-form msg)))
                    (cons "$/alive/surroundingFormBounds" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/form-bounds:surrounding-form msg)))

                    (cons "$/alive/getPackageForPosition" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/packages:for-position msg)))
                    (cons "$/alive/listPackages" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/packages:list-all msg)))
                    (cons "$/alive/removePackage" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/packages:remove-pkg msg)))

                    (cons "$/alive/listThreads" (lambda (deps state msg) (alive/session/handler/threads:new-list-all deps state msg)))
                    (cons "$/alive/killThread" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/threads:kill msg)))

                    (cons "$/alive/listAsdfSystems" (lambda (deps state msg) (declare (ignore state)) (alive/session/handler/asdf:new-list-all deps msg)))
                    (cons "$/alive/loadAsdfSystem" (lambda (deps state msg)
                                                       (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "ASDF")
                                                                                  (cdr (assoc :id msg))
                                                                                  (lambda ()
                                                                                      (alive/session/handler/asdf:new-load-system deps state msg)))))

                    (cons "$/alive/tryCompile" (lambda (deps state msg)
                                                   (declare (ignore state))
                                                   (spawn:new-thread "Try Compile"
                                                       (alive/session/handler/compile:new-try deps msg))))
                    (cons "$/alive/compile" (lambda (deps state msg)
                                                (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "Compile")
                                                                           (cdr (assoc :id msg))
                                                                           (lambda ()
                                                                               (alive/session/handler/compile:file msg)))))
                    (cons "$/alive/loadFile" (lambda (deps state msg)
                                                 (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "Load File")
                                                                            (cdr (assoc :id msg))
                                                                            (lambda ()
                                                                                (alive/session/handler/compile:load-file msg)))))

                    (cons "$/alive/symbol" (lambda (deps state msg) (declare (ignore deps)) (alive/session/handler/symbol:new-for-pos state msg)))
                    (cons "$/alive/unexportSymbol" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/symbol:do-unexport msg)))

                    (cons "$/alive/macroexpand" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/macro:expand msg)))
                    (cons "$/alive/macroexpand1" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/macro:expand-1 msg)))

                    (cons "$/alive/inspect" (lambda (deps state msg)
                                                (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "Inspect")
                                                                           (cdr (assoc :id msg))
                                                                           (lambda ()
                                                                               (alive/session/handler/inspect:do-inspect msg)))))
                    (cons "$/alive/inspectRefresh" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/inspect:refresh msg)))
                    (cons "$/alive/inspectClose" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/inspect:do-close msg)))
                    (cons "$/alive/inspectSymbol" (lambda (deps state msg)
                                                      (threads:new-run-in-thread deps state (or (cdr (assoc :method msg)) "Inspect")
                                                                                 (cdr (assoc :id msg))
                                                                                 (lambda ()
                                                                                     (alive/session/handler/inspect:do-symbol msg)))))
                    (cons "$/alive/inspectEval" (lambda (deps state msg) (declare (ignore deps state)) (threads:run-in-thread (or (cdr (assoc :method msg)) "Inspect")
                                                                                                                              (cdr (assoc :id msg))
                                                                                                                              (lambda ()
                                                                                                                                  (alive/session/handler/inspect:do-inspect-eval msg)))))
                    (cons "$/alive/inspectMacro" (lambda (deps state msg) (declare (ignore deps state)) (alive/session/handler/inspect:macro msg)))))


(declaim (ftype (function () state:state) create-session-state))
(defun create-session-state ()
    (state:create))


(declaim (ftype (function () deps:deps) create-deps))
(defun create-deps ()
    (deps:create :msg-handler (lambda (msg) (alive/session/message:handle msg))
                 :send-msg (lambda (msg) (alive/session/transport:send-msg msg))
                 :send-request (lambda (req) (alive/session/transport:send-request req))
                 :read-msg (lambda () (alive/session/transport:read-msg))
                 :list-all-threads (lambda () (alive/sys/threads:list-all))
                 :kill-thread (lambda (id) (alive/sys/threads:kill id))
                 :list-all-asdf (lambda () (alive/sys/asdf:list-all))
                 :load-asdf-system (lambda (&rest args) (apply 'alive/sys/asdf:load-system args))
                 :get-thread-id (lambda (thread) (alive/sys/threads:get-id thread))
                 :eval-fn (lambda (arg) (alive/sys/eval:eval-fn arg))
                 :macro-expand (lambda (txt pkg) (alive/macros:expand txt pkg))
                 :macro-expand-1 (lambda (txt pkg) (alive/macros:expand-1 txt pkg))
                 :try-compile (lambda (path) (alive/file:try-compile path))
                 :do-compile (lambda (path &key stdin-fn stdout-fn stderr-fn) (alive/file:do-compile path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))
                 :do-load (lambda (path &key stdin-fn stdout-fn stderr-fn) (alive/file:do-load path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))))


(declaim (ftype (function (&key (:input-stream flexi-streams:flexi-io-stream) (:output-stream T) (:state state:state)) deps:dependencies) new-create-deps))
(defun new-create-deps (&key input-stream output-stream state)
    (deps:new-create :msg-handler (lambda (deps msg) (alive/session/message:new-handle deps state *new-message-handlers* msg))
                     :send-msg (lambda (msg) (alive/session/transport:new-send-msg state output-stream msg))
                     :send-request (lambda (req) (alive/session/transport:new-send-request state output-stream req))
                     :read-msg (lambda () (alive/session/transport:new-read-msg input-stream))
                     :list-all-threads (lambda () (alive/sys/threads:list-all))
                     :kill-thread (lambda (id) (alive/sys/threads:kill id))
                     :list-all-asdf (lambda () (alive/sys/asdf:list-all))
                     :load-asdf-system (lambda (&rest args) (apply 'alive/sys/asdf:load-system args))
                     :get-thread-id (lambda (thread) (alive/sys/threads:get-id thread))
                     :eval-fn (lambda (arg) (alive/sys/eval:eval-fn arg))
                     :macro-expand (lambda (txt pkg) (alive/macros:expand txt pkg))
                     :macro-expand-1 (lambda (txt pkg) (alive/macros:expand-1 txt pkg))
                     :try-compile (lambda (path) (alive/file:try-compile path))
                     :do-compile (lambda (path &key stdin-fn stdout-fn stderr-fn) (alive/file:do-compile path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))
                     :do-load (lambda (path &key stdin-fn stdout-fn stderr-fn) (alive/file:do-load path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))))


(defun accept-conn ()
    (let* ((conn (usocket:socket-accept (socket *server*) :element-type '(unsigned-byte 8)))
           (state (state:create))
           (deps (new-create-deps :input-stream (flexi-streams:make-flexi-stream (usocket:socket-stream conn))
                                  :output-stream (usocket:socket-stream conn)
                                  :state state)))
        (session:new-start deps state)
        #+n (context:with-context (:input-stream (flexi-streams:make-flexi-stream (usocket:socket-stream conn))
                                                 :output-stream (usocket:socket-stream conn)
                                                 :destroy-fn (lambda ()
                                                                 (usocket:socket-close conn)))
                (alive/deps:with-deps (create-deps)
                    (handlers:with-handlers *message-handlers*
                        (session:start))))))

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
                                  (logger:*logger* logger)
                                  (*standard-output* stdout))
                                (listen-for-conns port)))
                        :name "Alive LSP Server")))

(defun stop ()
    (logger:info-msg "Stop server")

    (stop-server)

    (setf *server* nil))

(defun start (&key (port *default-port*))
    (logger:with-logging (logger:create *standard-output* logger:*info*)
        (let ((*server* (make-instance 'lsp-server)))
            (start-server port))))
