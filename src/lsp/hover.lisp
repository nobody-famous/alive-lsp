(defpackage :alive/lsp/hover
    (:use :cl)
    (:export :get-text)
    (:local-nicknames (:tokenizer :alive/parse/tokenizer)
                      (:packages :alive/packages)
                      (:token :alive/parse/token)
                      (:types :alive/types)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/hover)


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
                          (format NIL "HOVER ~A:~A~%" (token:get-text token3) (token:get-text token1))
                          #+n (symbol-with-pkg :name (token:get-text token1)
                                               :num-colons (length (token:get-text token2))
                                               :pkg-name (token:get-text token3)))

                      ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*quote*))
                          (format NIL "HOVER '~A~%" (token:get-text token1))
                          #+n (prefix-symbols "'" (symbol-no-pkg :name (token:get-text token1)
                                                                 :pkg-name (package-name *package*))))

                      ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*back-quote*))
                          (format NIL "HOVER `~A~%" (token:get-text token1))
                          #+n (prefix-symbols "`" (symbol-no-pkg :name (token:get-text token1)
                                                                 :pkg-name (package-name *package*))))

                      ((eq (token:get-type-value token1) types:*symbol*)
                          (format NIL "HOVER ~A~%" (token:get-text token1))
                          #+n (symbol-no-pkg :name (token:get-text token1)
                                             :pkg-name (package-name *package*)))

                      (T ""))))))
