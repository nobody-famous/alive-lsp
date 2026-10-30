(defpackage :alive/session/spawn
    (:use :cl)
    (:export :new-thread
             :thread))

(in-package :alive/session/spawn)


(defmacro new-thread (name &body body)
    (let ((stdin (gensym))
          (stdout (gensym)))

        `(let* ((,stdout *standard-output*)
                (,stdin *standard-input*))
             (bt:make-thread (lambda ()
                                 (let ((*standard-output* ,stdout)
                                       (*standard-input* ,stdin))
                                     (progn ,@body)))
                             :name ,name))))
