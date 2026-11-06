(defpackage :alive/sys/asdf
    (:use :cl)
    (:export :list-all
             :load-system)
    (:local-nicknames (:astreams :alive/sys/streams)))

(in-package :alive/sys/asdf)


(defun list-all ()
    (mapcar #'string-downcase (asdf:registered-systems)))


(defun load-system (&key name stdin-fn stdout-fn stderr-fn force)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (asdf:load-system name :force force)))
