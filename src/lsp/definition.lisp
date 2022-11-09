(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:packages :alive/packages)
                      (:utils :alive/lsp/utils)
                      (:forms :alive/parse/forms)))

(in-package :alive/lsp/definition)


(defun get-range-from-file (file source-path)
    (handler-case
            (with-open-file (in-stream file)
                (let ((forms (forms:from-stream in-stream)))
                    (forms:get-range-for-path forms source-path)))
        (T nil)))


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


(defun url-encode-filename (name)
    (let* ((raw-pieces (uiop:split-string name :separator "/\\"))
           (pieces (mapcar (lambda (piece)
                               (if (string= piece "")
                                   ""
                                   (format NIL "/~A" (url-encode piece))))
                           raw-pieces)))
        (apply #'concatenate 'string pieces)))


(defun lookup-sources (sym)
    (let ((types (list :class
                       :compiler-macro
                       :condition
                       :constant
                       :function
                       :generic-function
                       :macro
                       :method
                       :method-combination
                       :package
                       :setf-expander
                       :structure
                       :symbol-macro
                       :type
                       :alien-type
                       :variable
                       :declaration)))
        (reduce (lambda (out item)
                    (if out
                        out
                        (let* ((srcs (sb-introspect:find-definition-sources-by-name sym item))
                               (src (first srcs)))
                            (if (and src
                                     (sb-introspect:definition-source-pathname src))
                                src
                                nil))))
                types
            :initial-value nil)))


(defun get-symbol-location (name pkg-name)
    (let* ((sym (alive/symbols:lookup name pkg-name))
           (src (when sym
                      (lookup-sources sym)))
           (file (when src (sb-introspect:definition-source-pathname src)))
           (form-path (when src (sb-introspect:definition-source-form-path src))))

        (if file
            (list (url-encode-filename (namestring file))
                  (get-range-from-file file form-path))
            (list nil nil))))


(defun get-location (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)

                (utils:symbol-for-pos :text text :pos pos)

            (when (and name pkg-name)
                  (get-symbol-location name pkg-name)))))
