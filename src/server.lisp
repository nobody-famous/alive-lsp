(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:deps :alive/deps)
                      (:handlers :alive/session/handlers)
                      (:logger :alive/logger)
                      (:packet :alive/lsp/packet)
                      (:session :alive/session)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/server)


(defvar *default-port* 0)

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


(declaim (ftype (function (state:state) alive/session/handlers:list-of-handlers) get-message-handlers))
(defun get-message-handlers (state)
    (list (cons "initialize" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/init:request msg)))
          (cons "initialized" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/init:initialized state msg)))

          (cons "textDocument/completion" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:completion state msg)))
          (cons "textDocument/definition" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:definition state msg)))
          (cons "textDocument/didChange" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:did-change state msg)))
          (cons "textDocument/didClose" (lambda (deps msg) (declare (ignore deps)) (ignore-msg msg)))
          (cons "textDocument/didOpen" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:did-open state msg)))
          (cons "textDocument/didSave" (lambda (deps msg) (declare (ignore deps)) (ignore-msg msg)))
          (cons "textDocument/documentSymbol" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:doc-symbols state msg)))
          (cons "textDocument/hover" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:hover state msg)))
          (cons "textDocument/onTypeFormatting" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:on-type state msg)))
          (cons "textDocument/rangeFormatting" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:formatting state msg)))
          (cons "textDocument/references" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:references state msg)))
          (cons "textDocument/selectionRange" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:selection state msg)))
          (cons "textDocument/semanticTokens/full" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/document:sem-tokens state msg)))
          (cons "textDocument/signatureHelp" (lambda (deps msg) (declare (ignore deps))
                                                 (alive/session/handler/document:sig-help state msg)))

          (cons "$/setTrace" (lambda (deps msg) (declare (ignore deps)) (ignore-msg msg)))
          (cons "$/cancelRequest" (lambda (deps msg) (declare (ignore deps)) (ignore-msg msg)))

          (cons "$/alive/eval" (lambda (deps msg)
                                   (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "eval")
                                                          (cdr (assoc :id msg))
                                                          (lambda ()
                                                              (alive/session/handler/eval:handle deps state msg)))))
          (cons "$/alive/topFormBounds" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/form-bounds:top-form state msg)))
          (cons "$/alive/surroundingFormBounds" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/form-bounds:surrounding-form state msg)))

          (cons "$/alive/getPackageForPosition" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/packages:for-position state msg)))
          (cons "$/alive/listPackages" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/packages:list-all msg)))
          (cons "$/alive/removePackage" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/packages:remove-pkg msg)))

          (cons "$/alive/listThreads" (lambda (deps msg) (alive/session/handler/threads:list-all deps state msg)))
          (cons "$/alive/killThread" (lambda (deps msg) (alive/session/handler/threads:kill deps state msg)))

          (cons "$/alive/listAsdfSystems" (lambda (deps msg) (alive/session/handler/asdf:list-all deps msg)))
          (cons "$/alive/loadAsdfSystem" (lambda (deps msg)
                                             (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "ASDF")
                                                                    (cdr (assoc :id msg))
                                                                    (lambda ()
                                                                        (alive/session/handler/asdf:load-system deps state msg)))))

          (cons "$/alive/tryCompile" (lambda (deps msg)
                                         (spawn:new-thread "Try Compile"
                                             (alive/session/handler/compile:try deps msg))))
          (cons "$/alive/compile" (lambda (deps msg)
                                      (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "Compile")
                                                             (cdr (assoc :id msg))
                                                             (lambda ()
                                                                 (alive/session/handler/compile:file deps state msg)))))
          (cons "$/alive/loadFile" (lambda (deps msg)
                                       (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "Load File")
                                                              (cdr (assoc :id msg))
                                                              (lambda ()
                                                                  (alive/session/handler/compile:load-file deps state msg)))))

          (cons "$/alive/symbol" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/symbol:for-pos state msg)))
          (cons "$/alive/unexportSymbol" (lambda (deps msg) (alive/session/handler/symbol:do-unexport deps state msg)))

          (cons "$/alive/macroexpand" (lambda (deps msg) (alive/session/handler/macro:expand deps msg)))
          (cons "$/alive/macroexpand1" (lambda (deps msg) (alive/session/handler/macro:expand-1 deps msg)))

          (cons "$/alive/inspect" (lambda (deps msg)
                                      (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "Inspect")
                                                             (cdr (assoc :id msg))
                                                             (lambda ()
                                                                 (alive/session/handler/inspect:do-inspect deps state msg)))))
          (cons "$/alive/inspectRefresh" (lambda (deps msg) (alive/session/handler/inspect:refresh deps state msg)))
          (cons "$/alive/inspectClose" (lambda (deps msg) (declare (ignore deps)) (alive/session/handler/inspect:do-close state msg)))
          (cons "$/alive/inspectSymbol" (lambda (deps msg)
                                            (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "Inspect")
                                                                   (cdr (assoc :id msg))
                                                                   (lambda ()
                                                                       (alive/session/handler/inspect:do-symbol deps state msg)))))
          (cons "$/alive/inspectEval" (lambda (deps msg) (threads:run-in-thread deps state (or (cdr (assoc :method msg)) "Inspect")
                                                                                (cdr (assoc :id msg))
                                                                                (lambda ()
                                                                                    (alive/session/handler/inspect:do-inspect-eval deps state msg)))))
          (cons "$/alive/inspectMacro" (lambda (deps msg) (alive/session/handler/inspect:macro deps state msg)))))


(declaim (ftype (function () state:state) create-session-state))
(defun create-session-state ()
    (state:create))


(declaim (ftype (function (&key (:input-stream flexi-streams:flexi-io-stream) (:output-stream T) (:state state:state)) deps:dependencies) create-deps))
(defun create-deps (&key input-stream output-stream state)
    (let ((handlers (get-message-handlers state)))
        (deps:create :msg-handler (lambda (deps msg) (alive/session/message:handle deps state handlers msg))
                     :send-msg (lambda (msg) (alive/session/transport:send-msg state output-stream msg))
                     :send-request (lambda (req) (alive/session/transport:send-request state output-stream req))
                     :read-msg (lambda () (alive/session/transport:read-msg input-stream))
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
                     :do-load (lambda (path &key stdin-fn stdout-fn stderr-fn) (alive/file:do-load path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)))))


(defun accept-conn (server log)
    (let* ((conn (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8)))
           (state (state:create :log log))
           (deps (create-deps :input-stream (flexi-streams:make-flexi-stream (usocket:socket-stream conn))
                              :output-stream (usocket:socket-stream conn)
                              :state state)))
        (session:start deps state)))


(defun wait-for-conn (server log)
    (usocket:wait-for-input (socket server))

    (when (and (running server)
               (usocket::state (socket server)))
          (accept-conn server log)))


(defun wake-up-accept (server)
    (ignore-errors
        (let ((conn (usocket:socket-connect "127.0.0.1" (usocket:get-local-port (socket server)))))
            (usocket:socket-close conn))))


(defun stop-server (server)
    (bt:with-recursive-lock-held ((lock server))
        (setf (running server) nil)

        (when (socket server)
              (wake-up-accept server)
              (usocket:socket-close (socket server))
              (setf (socket server) nil))))


(defun listen-for-conns (server log port)
    (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (format T "[~A][STARTING] Started on port ~A~%" (alive/utils:get-timestamp) (usocket:get-local-port socket))

        (unwind-protect
                (progn (setf (socket server) socket)
                       (setf (running server) T)

                       (loop :while (running server)
                             :do (wait-for-conn server log)))
            (stop-server server))))


(defun start-server (server log port)
    (let ((stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (listen-for-conns server log port)))
                        :name "Alive LSP Server")))


(defun stop (server)
    ; (logger:info-msg "Stop server")

    (stop-server server))


(defun start (&key (port *default-port*))
    (let ((log (logger:create *standard-output* logger:*info*))
          (server (make-instance 'lsp-server)))
        (start-server server log port)))
