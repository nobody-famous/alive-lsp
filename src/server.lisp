(defpackage :alive/server
    (:use :cl)
    (:export :start)
)

(in-package :alive/server)


(defvar *default-port* 25483)


(defun start (&key (port *default-port*))
    (format T "Start on port ~A~%" port)

    (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address T)))
        (unwind-protect
                (let* ((conn (usocket:socket-accept socket))
                       (line (read-line (usocket:socket-stream conn)))
                      )
                    (format T "LINE: ~A~%" line)
                )
            (usocket:socket-close socket)
        )))
