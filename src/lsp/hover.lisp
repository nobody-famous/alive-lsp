(defpackage :alive/lsp/hover
    (:use :cl)
    (:export :get-text)
    (:local-nicknames (:tokenizer :alive/parse/tokenizer)
                      (:packages :alive/packages)
                      (:token :alive/parse/token)
                      (:types :alive/types)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/hover)


(defun get-symbol-doc (name pkg-name)
    (let* ((sym (alive/symbols:lookup name pkg-name)))
        (when sym
              (with-output-to-string (str)
                  (cond ((alive/symbols:get-lambda-list name pkg-name)
                            (describe sym str))
                        ((boundp sym)
                            (format str "~A~%" (symbol-value sym)))
                        (T (describe sym str)))))))


(defun get-text (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (utils:symbol-for-pos text pos)
            (when (and name pkg-name)
                  (get-symbol-doc name pkg-name)))))
