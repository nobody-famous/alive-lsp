(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:packages :alive/packages)
                      (:utils :alive/lsp/utils)
                      (:forms :alive/parse/forms)))

(in-package :alive/lsp/definition)


(defun get-symbol-location (name pkg-name)
    (let* ((sym (alive/symbols:lookup name pkg-name))
           (src (when sym
                      (sb-introspect:find-definition-sources-by-name sym :function)))
           (def-src (when src (first src))))

        (if def-src
            (list
                (sb-introspect:definition-source-pathname def-src)
                (sb-introspect:definition-source-form-path def-src))
            (list nil nil))))


(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)

                (utils:symbol-for-pos :text text :pos pos)

            (when (and name pkg-name)
                  (get-symbol-location name pkg-name)))))
