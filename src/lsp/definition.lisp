(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:packages :alive/packages)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/definition)


(defun get-location-for-name (name pkg-name)
    (let ((sym (alive/symbols:lookup name pkg-name)))
        (alive/symbols:get-location sym)))


(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (utils:symbol-for-pos text pos)
            (when (and name pkg-name)
                  (get-location-for-name name pkg-name)))))
