(defpackage :alive/server
    (:use :cl)
    (:export :create
             :stop
             :start)

    (:local-nicknames (:parse :alive/lsp/parse)
                      (:session :alive/session)))

(in-package :alive/server)


(defvar *default-port* 25483)


(defclass lsp-server ()
    ((running :accessor running
              :initform nil
              :initarg :running)
     (socket :accessor socket
             :initform nil
             :initarg :socket)))


(defun accept-conn (socket)
    (let* ((conn (usocket:socket-accept socket))
           (stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (format T "Connection received~%")
                                (session:start conn))))))


(defun wait-for-conn (socket)
    (format T "Waiting for connection~%")

    (let ((ready (usocket:wait-for-input socket)))
        (format T "Connection ready ~A~%" ready)
        (when ready
              (accept-conn ready))))


(defun stop (server)
    (format T "Stop server~%")

    (when (socket server)
          (usocket:socket-close (socket server)))

    (setf (running server) nil)
    (setf (socket server) nil))


(defun start (server &key (port *default-port*))
    (when (running server)
          (error "Server already running"))

    (let ((stdout *standard-output*)
          (socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (format T "Started on port ~A~%" (usocket:get-local-port socket))

        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (unwind-protect
                                        (progn (setf (socket server) socket)
                                               (setf (running server) T)

                                               (loop :while (running server)
                                                     :do (wait-for-conn socket)))
                                    (stop server)))))))


(defun create ()
    (make-instance 'lsp-server))
