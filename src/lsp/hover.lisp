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

        (if sym
            (with-output-to-string (str)
                (describe sym str))
            "")))


(defun get-text (&key text pos)
    (let* ((raw-tokens (tokenizer:from-stream (make-string-input-stream text)))
           (tokens (utils:find-tokens raw-tokens pos))
           (pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (if (zerop (length tokens))
            ""
            (destructuring-bind (token1 token2 token3) tokens
                (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*colons*)
                            (eq (token:get-type-value token3) types:*symbol*))
                          (get-symbol-doc (token:get-text token1)
                                          (token:get-text token3)))

                      ((eq (token:get-type-value token1) types:*symbol*)
                          (get-symbol-doc (token:get-text token1)
                                          pkg-name))

                      (T ""))))))
