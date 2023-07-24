(defpackage :alive/server
    (:use :cl)
    (:export :stop
             :start)

    (:local-nicknames (:logger :alive/logger)
                      (:parse :alive/lsp/parse)
                      (:session :alive/session)))

(in-package :alive/server)


(defvar *default-port* 0)
(defparameter *server* nil)

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
    (format T "***** accept-conn~%")
    (let* ((conn (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8)))
           (session (session:create :conn conn)))

        (session:add-listener session
                              (make-instance 'session:listener
                                  :on-done (lambda ()
                                               (usocket:socket-close conn)
                                               (setf (sessions server)
                                                   (remove session (sessions server))))))
        (format T "***** before start session~%")
        (session:start session)
        (format T "***** after start session~%")

        (push session (sessions server))))


(defun wait-for-conn (server)
    (format T "***** wait-for-conn ~A~%" server)
    (usocket:wait-for-input (socket server))

    (format T "***** wait-for-conn running ~A~%" (running server))
    (format T "***** wait-for-conn state ~A~%" (usocket::state (socket server)))
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
        (format T "***** listen before log~%")
        (logger:msg logger:*info* "Started on port ~A~%" (usocket:get-local-port socket))
        (format T "***** listen after log~%")

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
                        :name "Alive LSP Server")))

(defun stop ()
    (format T "***** stop server~%")
    (logger:msg logger:*info* "Stop server~%")

    (stop-server *server*)

    (setf *server* nil))


(defun create-server (logger)
    (let ((server (make-instance 'lsp-server)))
        (setf (logger server) logger)
        (setf *server* server)))


(defun start (&key (port *default-port*))
    (let ((logger (logger:create *standard-output* logger:*info*)))
        (if *server*
            (logger:msg logger logger:*error* "Server already running")
            (progn (create-server logger)
                   (logger:init *standard-output* logger:*info*)
                   (format T "***** before start server~%")
                   (start-server *server* port)
                   (format T "***** after start server~%"))))

    #+n (if *server*
            (logger:msg logger:*error* "Server already running")

            (progn (logger:init *standard-output* logger:*info*)
                   (setf *server* (make-instance 'lsp-server))
                   (format T "***** before start~%")
                   (start-server *server* port)
                   (format T "***** after start~%"))))
