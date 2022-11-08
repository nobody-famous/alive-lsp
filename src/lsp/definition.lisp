(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:packages :alive/packages)
                      (:utils :alive/lsp/utils)
                      (:forms :alive/parse/forms)))

(in-package :alive/lsp/definition)


(defun get-range-from-file (file source-path)
    (with-open-file (in-stream file)
        (let ((forms (forms:from-stream in-stream)))
            (forms:get-range-for-path forms source-path))))


(defun needs-encoding (char)
    (eq char #\:))


(defun encode-char (char)
    (if (needs-encoding char)
        (format nil "%~2,'0X" (char-code char))
        (string char)))


(defun url-encode (str)
    (let ((chars (map 'list (lambda (char)
                                (encode-char char))
                     str)))
        (apply #'concatenate 'string chars)))


(defun get-symbol-location (name pkg-name)
    (let* ((sym (alive/symbols:lookup name pkg-name))
           (src (when sym
                      (sb-introspect:find-definition-sources-by-name sym :function)))
           (def-src (when src (first src)))
           (file (when def-src (sb-introspect:definition-source-pathname def-src)))
           (form-path (when def-src (sb-introspect:definition-source-form-path def-src))))

        (if file
            (list (format nil "file:///~A" (url-encode (namestring file))) (get-range-from-file file form-path))
            (list nil nil))))


(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)

                (utils:symbol-for-pos :text text :pos pos)

            (when (and name pkg-name)
                  (get-symbol-location name pkg-name)))))
