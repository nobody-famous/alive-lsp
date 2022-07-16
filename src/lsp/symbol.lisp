(defpackage :alive/lsp/symbol
    (:use :cl)
    (:export :for-pos)
    (:local-nicknames (:tokenizer :alive/parse/tokenizer)
                      (:packages :alive/packages)
                      (:token :alive/parse/token)
                      (:types :alive/types)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/symbol)


(defun for-pos (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)

                (utils:symbol-for-pos :text text :pos pos)

            (when (and name pkg-name)
                  (list name pkg-name)))))
