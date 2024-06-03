(defpackage :alive/asdf
    (:use :cl)
    (:export :list-systems
             :load-system)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/asdf)


(declaim (ftype (function () list) list-systems))
(defun list-systems ()
    (mapcar #'string-downcase (asdf:registered-systems)))


(declaim (ftype (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) *) load-system))
(defun load-system (&key name stdin-fn stdout-fn stderr-fn force)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (asdf:load-system name :force force)))
