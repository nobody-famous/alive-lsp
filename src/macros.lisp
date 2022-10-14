(defpackage :alive/macros
    (:use :cl)
    (:export :expand
             :expand-1)
    (:local-nicknames (:pkgs :alive/packages)))

(in-package :alive/macros)


(defun do-expand (fn text)
    (handler-case
            (funcall fn (read (make-string-input-stream text)))
        (T ()
           nil)))

(defun expand (text pkg-name)
    (let* ((pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (macroexpand (read (make-string-input-stream text)))
        #+n (do-expand 'macroexpand text)))


(defun expand-1 (text)
    (do-expand 'macroexpand-1 text))
