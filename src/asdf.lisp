(defpackage :alive/asdf
    (:use :cl)
    (:export :list-systems
             :load-system)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/asdf)


(defun list-systems ()
    (mapcar #'string-downcase (asdf:registered-systems)))


(defun load-system (&key name stdin-fn stdout-fn stderr-fn force)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (asdf:load-system name :force force)))
