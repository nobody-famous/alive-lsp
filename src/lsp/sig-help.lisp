(defpackage :alive/lsp/sig-help
    (:use :cl)
    (:export :signatures)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:pkgs :alive/packages)
                      (:pos :alive/position)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/lsp/sig-help)


(defun get-param-info (name)
    (let ((info (make-hash-table)))
        (setf (gethash "label" info) name)
        (setf (gethash "documentation" info) "Some param docs")
        info))


(defun get-sig-info (name)
    (let ((info (make-hash-table)))
        (setf (gethash "label" info) name)
        (setf (gethash "documentation" info) "This is some doc stuff")
        (setf (gethash "parameters" info) (list (get-param-info (list 4 5)) (get-param-info (list 6 7))))
        (setf (gethash "activeParameter" info) 1)
        info))


(declaim (ftype (function ((or null hash-table) (or null hash-table) (or null hash-table)) (values (or null string) &optional)) get-fn-package))
(defun get-fn-package (token1 token2 token3)
    (when (and (eq (token:get-type-value token1) types:*symbol*)
               (eq (token:get-type-value token2) types:*colons*)
               (eq (token:get-type-value token3) types:*symbol*))
          (token:get-text token3)))


(declaim (ftype (function ((or null hash-table)) (values (or null string) &optional)) get-fn-name))
(defun get-fn-name (token)
    (when (eq (token:get-type-value token) types:*symbol*)
          (token:get-text token)))


(declaim (ftype (function ((or null hash-table) (or null hash-table) (or null hash-table)) (or null hash-table)) get-sig))
(defun get-sig (token1 token2 token3)
    (let* ((pkg-name (get-fn-package token1 token2 token3))
           (fn-name (get-fn-name token1))
           (lambda-list (symbols:get-lambda-list fn-name pkg-name)))
        (format T "***** GET-SIG ~A ~A ~A~%" fn-name lambda-list (type-of lambda-list))))


(declaim (ftype (function (&key (:text string) (:pos pos:text-position)) (or null cons)) signatures))
(defun signatures (&key text pos)
    (let* ((forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (tokens (tokenizer:from-stream (make-string-input-stream text)))
           (top-form (forms:get-top-form forms pos))
           (outer-form (forms:get-outer-form top-form pos))
           (name-form (when (hash-table-p outer-form)
                            (first (gethash "kids" outer-form))))
           (name-tokens (when (hash-table-p name-form)
                              (alive/lsp/utils:find-tokens tokens (gethash "end" name-form))))
           (pkg-name (alive/packages:for-pos text pos))
           (pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (list (get-sig-info "foo x y")
              (get-sig-info "foo a b"))

        (when (>= (length name-tokens) 3)
              (destructuring-bind (token1 token2 token3)
                      name-tokens
                  (get-sig token1 token2 token3)
                  nil))))
