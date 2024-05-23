(defpackage :alive/session/handler/packages
    (:use :cl)
    (:export :for-position
             :list-all
             :remove-pkg)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

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

        (utils:result id "package" pkg)))


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (let ((id (cdr (assoc :id msg)))
          (pkgs (packages:list-all)))
        (utils:result id "packages" pkgs)))


(declaim (ftype (function (cons) hash-table) remove-pkg))
(defun remove-pkg (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :name params))))

        (packages:do-remove pkg-name)
        (lsp-msg:create-response id :result-value T)))
