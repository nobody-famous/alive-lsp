(defpackage :alive/macros
    (:use :cl)
    (:export :expand
             :expand-1)
    (:local-nicknames (:pkgs :alive/packages)))

(in-package :alive/macros)


(defun do-expand (fn text pkg-name)
    (handler-case
            (let* ((pkg (pkgs:lookup pkg-name))
                   (*package* (if pkg pkg *package*)))

                (funcall fn (read (make-string-input-stream text))))
        (T ()
           nil)))

(defun expand (text pkg-name)
    (do-expand 'macroexpand text pkg-name))


(defun expand-1 (text pkg-name)
    (do-expand 'macroexpand-1 text pkg-name))
