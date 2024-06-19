(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/definition)


(declaim (ftype (function (string string) (or null loc:text-location)) get-location-for-name))
(defun get-location-for-name (name pkg-name)
    (let ((sym (alive/symbols:lookup name pkg-name)))
        (alive/symbols:get-location sym)))


(declaim (ftype (function (&key (:text string) (:pos pos:text-position)) (or null loc:text-location)) get-location))
(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)

                (utils:symbol-for-pos :text text :pos pos)

            (when (and name pkg-name)
                  (get-location-for-name name pkg-name)))))
