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


(defun get-param-info (label)
    (let ((info (make-hash-table :test #'equalp)))
        (setf (gethash "label" info) label)
        info))


(declaim (ftype (function ((or null hash-table) (or null hash-table) (or null hash-table)) (values (or null string) &optional)) get-fn-package))
(defun get-fn-package (token1 token2 token3)
    (if (and (eq (token:get-type-value token1) types:*symbol*)
             (eq (token:get-type-value token2) types:*colons*)
             (eq (token:get-type-value token3) types:*symbol*))
        (token:get-text token3)
        (package-name *package*)))


(declaim (ftype (function ((or null hash-table)) (values (or null string) &optional)) get-fn-name))
(defun get-fn-name (token)
    (when (eq (token:get-type-value token) types:*symbol*)
          (string-upcase (token:get-text token))))


(declaim (ftype (function (string string) string) generate-label))
(defun generate-label (fn-name pkg-name)
    (loop :with label := fn-name
          :with lambda-list := (symbols:get-lambda-list fn-name pkg-name)
          :with params := ()
          :with index := (length label)
          :with add-params := T

          :for item :in lambda-list
          :do (let ((item-str (format nil "~A" item)))
                  (when (char= #\& (char item-str 0))
                        (setf add-params nil))
                  (when add-params
                        (setf params (push (get-param-info (list (+ 1 index)
                                                                 (+ 1 index (length item-str))))
                                           params))
                        (setf index (+ 1 index (length item-str))))
                  (setf label (format nil "~A ~A" label item)))

          :finally (return (values label (or (reverse params) (make-array 0))))))


(declaim (ftype (function (number string string) (or null hash-table)) get-sig-info))
(defun get-sig-info (active-param fn-name pkg-name)
    (multiple-value-bind (label params)
            (generate-label fn-name pkg-name)
        (when (or (zerop (length params))
                  (< active-param (length params)))
              (let ((doc (or (documentation (symbols:lookup fn-name pkg-name) 'function) ""))
                    (info (make-hash-table :test #'equalp)))
                  (setf (gethash "label" info) label)
                  (setf (gethash "documentation" info) doc)
                  (setf (gethash "parameters" info) params)
                  (setf (gethash "activeParameter" info) active-param)
                  info))))


(declaim (ftype (function (pos:text-position number (or null hash-table) (or null hash-table) (or null hash-table)) (or null hash-table)) get-sig))
(defun get-sig (pos active-param token1 token2 token3)
    (let* ((pkg-name (get-fn-package token1 token2 token3))
           (fn-name (get-fn-name token1)))
        (when (and (pos:less-than (token:get-start token1) pos)
                   (or (symbols:function-p fn-name pkg-name) (symbols:macro-p fn-name pkg-name))
                   fn-name
                   pkg-name)
              (get-sig-info active-param fn-name pkg-name))))


(declaim (ftype (function (pos:text-position hash-table) number) get-active-parameter))
(defun get-active-parameter (pos form)
    (loop :with param := 0
          :for kid :in (cdr (gethash "kids" form))
          :do (when (pos:less-than (gethash "end" kid) pos)
                    (incf param))
          :finally (return param)))


(declaim (ftype (function (&key (:text string) (:pos pos:text-position)) (values (or null cons) &optional)) signatures))
(defun signatures (&key text pos)
    (let* ((forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (tokens (tokenizer:from-stream (make-string-input-stream text)))
           (top-form (forms:get-top-form forms pos))
           (outer-form (forms:get-outer-form top-form pos))
           (name-form (when (hash-table-p outer-form)
                            (first (gethash "kids" outer-form))))
           (active-param (if (hash-table-p outer-form)
                             (get-active-parameter pos outer-form)
                             0))
           (name-tokens (when (hash-table-p name-form)
                              (alive/lsp/utils:find-tokens tokens (gethash "end" name-form))))
           (pkg-name (alive/packages:for-pos text pos))
           (pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (when (>= (length name-tokens) 3)
              (destructuring-bind (token1 token2 token3)
                      name-tokens
                  (let ((sig (get-sig pos active-param token1 token2 token3)))
                      (when sig
                            (list sig)))))))
