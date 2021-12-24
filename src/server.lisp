(defpackage :alive/server
    (:use :cl)
    (:export :create
             :stop
             :start)

    (:local-nicknames (:parse :alive/lsp/parse)
                      (:socket-pair :alive/socket-pair)
                      (:session :alive/session)))

(in-package :alive/server)


(defvar *default-port* 25483)


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


(defun accept-conn (server)
    (let* ((conn (usocket:socket-accept (socket server)))
           (session (session:start conn)))
        (format T "Connection received~%")
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
        (format T "Started on port ~A~%" (usocket:get-local-port socket))

        (unwind-protect
                (progn (setf (socket server) socket)
                       (setf (running server) T)

                       (loop :while (running server)
                             :do (wait-for-conn server)))
            (stop-server server))))


(defun start-server (server port)
    (let ((stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (listen-for-conns server port)))
                        :name "Main Loop")
        server))


(defun stop (server)
    (format T "Stop server~%")
    (stop-server server))


(defun start (server &key (port *default-port*))
    (if (running server)
        (format T "Server already running")
        (start-server server port)))


(defun create ()
    (make-instance 'lsp-server))
