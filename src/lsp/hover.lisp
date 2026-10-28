(defpackage :alive/lsp/hover
    (:use :cl)
    (:export :get-text)
    (:local-nicknames (:tokenizer :alive/parse/tokenizer)
                      (:packages :alive/packages)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/hover)


(defun get-symbol-doc (name pkg-name)
    (let* ((sym (symbols:lookup name pkg-name)))
        (when sym
              (with-output-to-string (str)
                  (cond ((symbols:get-lambda-list name pkg-name)
                            (describe sym str))
                        ((boundp sym)
                            (format str "~A~%" (symbol-value sym)))
                        (T (describe sym str)))))))


(defun get-text (&key forms pos)
    (let* ((pkg-name (packages:xyz-for-pos forms pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (symbols:for-pos forms pos)
            (when (and name pkg-name)
                  (get-symbol-doc name pkg-name)))))
