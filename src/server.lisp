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
     (pair :accessor pair
           :initform nil
           :initarg :pair)
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


(defun wait-for-conn (server)
    (format T "Waiting for connection~%")
    (format T "  ~A~%" (first (pair server)))

    (let* ((sockets (list (socket server) (first (pair server))))
           (inputs (usocket:wait-for-input sockets)))
        (format T "Connections ready ~A~%" inputs)
        (loop :for ready :in inputs :do
                  (format T "READY ~A~%" ready)
                  (when (eq ready (socket server))
                        (accept-conn ready)))))


(defun stop (server)
    (format T "Stop server~%")

    (when (socket server)
          (format T "Closing socket~%")
          (usocket:socket-close (socket server))
          (format T "Socket closed~%"))

    (when (pair server)
          (format T "Sending wake up~%")
          (write "WAKE UP"
                 :stream (usocket:socket-stream (second (pair server))))
          (force-output (usocket:socket-stream (second (pair server)))))

    (setf (running server) nil)
    (setf (socket server) nil)
    (setf (pair server) nil))


(defun create-socket-pair ()
    (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address T))
           (write-side (usocket:socket-connect "127.0.0.1" (usocket:get-local-port listener)))
           (read-side (usocket:socket-accept listener)))
        (usocket:socket-close listener)
        (list read-side write-side)))


(defun start (server &key (port *default-port*))
    (when (running server)
          (error "Server already running"))

    (let ((stdout *standard-output*)
          (socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (format T "Started on port ~A~%" (usocket:get-local-port socket))

        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (unwind-protect
                                        (progn (setf (pair server) (create-socket-pair))
                                               (setf (socket server) socket)
                                               (setf (running server) T)

                                               (loop :while (running server)
                                                     :do (wait-for-conn server)))
                                    (progn (format T "Top level calling stop~%")
                                           (stop server))))))))


(defun create ()
    (make-instance 'lsp-server))
