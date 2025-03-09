(defpackage :alive/session/spawn
    (:use :cl)
    (:export :new-thread
             :spawned-thread
             :thread))

(in-package :alive/session/spawn)


(define-condition spawned-thread ()
        ((thread :accessor thread
                 :initform nil
                 :type bt:thread
                 :initarg :thread))
    (:report (lambda (condition stream) (format stream "THREAD ~A" (thread condition)))))


(defmacro new-thread (name &body body)
    (let ((stdin (gensym))
          (stdout (gensym)))

        `(let* ((,stdout *standard-output*)
                (,stdin *standard-input*)
                (thread (bt:make-thread (lambda ()
                                            (let ((*standard-output* ,stdout)
                                                  (*standard-input* ,stdin))
                                                (progn ,@body)))
                                        :name ,name)))
             (signal (make-condition 'spawned-thread :thread thread))
             nil)))
