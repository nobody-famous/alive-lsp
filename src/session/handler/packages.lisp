(defpackage :alive/session/handler/packages
    (:use :cl)
    (:export :list-all
             :for-position
             :xyz-for-position
             :remove-pkg)
    (:local-nicknames (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/packages)


(defun for-position (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (pkg (packages:for-pos text pos)))

        (utils:result id "package" pkg)))


(defun xyz-for-position (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (forms (state:get-file-forms state uri))
           (pkg (packages:xyz-for-pos forms pos)))

        (utils:result id "package" pkg)))


(defun list-all (msg)
    (let ((id (cdr (assoc :id msg)))
          (pkgs (packages:list-all)))
        (utils:result id "packages" pkgs)))


(defun remove-pkg (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :name params))))

        (packages:do-remove pkg-name)
        (lsp-msg:create-response id :result-value T)))
