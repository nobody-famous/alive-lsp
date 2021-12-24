(defpackage :alive/server
    (:use :cl)
    (:export :create
             :stop
             :start)

    (:local-nicknames (:logger :alive/logger)
                      (:parse :alive/lsp/parse)
                      (:session :alive/session)
                      (:socket-pair :alive/socket-pair)))

(in-package :alive/server)


(defvar *default-port* 25483)


(defclass lsp-server ()
    ((running :accessor running
              :initform nil
              :initarg :running)
     (logger :accessor logger
             :initform nil
             :initarg :logger)
     (lock :accessor lock
           :initform (bt:make-recursive-lock)
           :initarg :lock)
     (sessions :accessor sessions
               :initform nil
               :initarg :sessions)
     (socket :accessor socket
             :initform nil
             :initarg :socket)))


(defun accept-conn (server)
    (let* ((conn (usocket:socket-accept (socket server)))
           (session (session:start (logger server) conn)))
        (logger:info-msg (logger server) "Connection received~%")
        (push session (sessions server))))


(defun wait-for-conn (server)
    (usocket:wait-for-input (socket server))

    (when (and (running server)
               (usocket::state (socket server)))
          (accept-conn server)))


(defun wake-up-accept (server)
    (ignore-errors
     (let ((conn (usocket:socket-connect "127.0.0.1" (usocket:get-local-port (socket server)))))
         (usocket:socket-close conn))))


(defun stop-server (server)
    (bt:with-recursive-lock-held ((lock server))
                                 (setf (running server) nil)

                                 (loop :for session :in (sessions server) :do
                                           (session:stop session))

                                 (setf (sessions server) nil)

                                 (when (socket server)
                                       (wake-up-accept server)
                                       (usocket:socket-close (socket server))
                                       (setf (socket server) nil))))


(defun listen-for-conns (server port)
    (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (logger:info-msg (logger server) "Started on port ~A~%" (usocket:get-local-port socket))

        (unwind-protect
                (progn (setf (socket server) socket)
                       (setf (running server) T)

                       (loop :while (running server)
                             :do (wait-for-conn server)))
            (stop-server server))))


(defun start-server (server port)
    (bt:make-thread (lambda () (listen-for-conns server port))
                    :name "Main Loop")

    (setf (logger server) (logger:create *standard-output* logger:*trace*))

    server)


(defun stop (server)
    (logger:info-msg (logger server) "Stop server~%")
    (stop-server server))


(defun start (server &key (port *default-port*))
    (if (running server)
        (logger:error-msg (logger server) "Server already running")
        (start-server server port)))


(defun create ()
    (make-instance 'lsp-server))
