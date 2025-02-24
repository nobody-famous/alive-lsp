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
          (stdout (gensym))
          (logger (gensym))
          (state (gensym))
          (context (gensym))
          (handlers (gensym)))

        `(let* ((,stdout *standard-output*)
                (,stdin *standard-input*)
                (,logger alive/logger:*logger*)
                (,state alive/session/state::*state*)
                (,context alive/context::*context*)
                (,handlers alive/session/handlers::*handlers*)
                (thread (bt:make-thread (lambda ()
                                            (let ((*standard-output* ,stdout)
                                                  (*standard-input* ,stdin)
                                                  (alive/logger:*logger* ,logger)
                                                  (alive/session/state::*state* ,state)
                                                  (alive/context::*context* ,context)
                                                  (alive/session/handlers::*handlers* ,handlers))
                                                (progn ,@body)))
                                        :name ,name)))
             (signal (make-condition 'spawned-thread :thread thread))
             nil)))
