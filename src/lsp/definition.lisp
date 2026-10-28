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


(defun get-location (&key forms pos)
    (let* ((pkg-name (packages:xyz-for-pos forms pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (sym:for-pos forms pos)
            (when (and name pkg-name)
                  (get-location-for-name name pkg-name)))))
