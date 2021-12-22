(defpackage :alive/server
    (:use :cl)
    (:export :create
             :stop
             :start)

    (:local-nicknames (:parse :alive/lsp/parse)
                      (:session :alive/session)))

(in-package :alive/server)


(defvar *default-port* 25483)


(defclass socket-pair ()
    ((reader :accessor reader
             :initform nil
             :initarg :reader)
     (writer :accessor writer
             :initform nil
             :initarg :writer)))


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
    (let* ((wake-up (reader (pair server)))
           (inputs (usocket:wait-for-input (list (socket server) wake-up))))
        (loop :for ready :in inputs :do
                  (when (eq ready (socket server))
                        (accept-conn (socket server))))))


(defun stop-server (server)
    (when (socket server)
          (usocket:socket-close (socket server))
          (setf (socket server) nil))

    (when (pair server)
          (write "WAKE UP"
                 :stream (usocket:socket-stream (writer (pair server))))
          (force-output (usocket:socket-stream (writer (pair server)))))

    (setf (running server) nil)
    (setf (pair server) nil))


(defun start-server (server port)
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
                                    (stop-server server)))))))


(defun stop (server)
    (format T "Stop server~%")
    (stop-server server))


(defun create-socket-pair ()
    (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address T))
           (port (usocket:get-local-port listener))
           (write-side (usocket:socket-connect "127.0.0.1" port))
           (read-side (usocket:socket-accept listener)))
        (usocket:socket-close listener)

        (make-instance 'socket-pair
                       :reader read-side
                       :writer write-side)))


(defun start (server &key (port *default-port*))
    (if (running server)
        (format T "Server already running")
        (start-server server port)))


(defun create ()
    (make-instance 'lsp-server))
