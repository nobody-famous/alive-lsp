(defpackage :alive/session/handler/packages
    (:use :cl)
    (:export :for-position)
    (:local-nicknames (:packages :alive/packages)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/packages)


(declaim (ftype (function (cons) hash-table) for-position))
(defun for-position (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text uri) ""))
           (pkg (packages:for-pos text pos)))

        (alive/session/handler/utils:result id "package" pkg)))
