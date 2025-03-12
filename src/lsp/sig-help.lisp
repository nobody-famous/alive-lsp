(defpackage :alive/lsp/sig-help
    (:use :cl)
    (:export :signatures))

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


(defun signatures (&key text pos)
    (declare (ignore text pos))
    (list (get-sig-info "foo x y")
          (get-sig-info "foo a b")))
