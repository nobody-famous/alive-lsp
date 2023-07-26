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


(defun accept-conn ()
    (let* ((conn (usocket:socket-accept (socket *server*) :element-type '(unsigned-byte 8)))
           (session (session:create :conn conn
                                    :logger (logger *server*))))

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
          (server *server*))
        (bt:make-thread (lambda ()
                            (let ((*server* server))
                                (logger:with-logging (logger *server*)
                                    (let ((*standard-output* stdout))
                                        (listen-for-conns port)))))
                        :name "Alive LSP Server")))

(defun stop ()
    (logger:info-msg "Stop server~%")

    (stop-server)

    (setf *server* nil))


(defun start (&key (port *default-port*))
    (let* ((logger (logger:create *standard-output* logger:*info*))
           (*server* (make-instance 'lsp-server :logger logger)))
        (start-server port)))
