(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:packages :alive/packages)
                      (:sym :alive/symbols)))

(in-package :alive/lsp/definition)


(defun get-location-for-name (name pkg-name)
    (let ((sym (sym:lookup name pkg-name)))
        (sym:get-location sym)))


(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (sym:for-pos text pos)
            (when (and name pkg-name)
                  (get-location-for-name name pkg-name)))))
