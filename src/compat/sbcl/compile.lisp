(defpackage :alive/compile
    (:use :cl)
    (:export :file)
)

(in-package :alive/compile)


(defun fatal-error (out-fn)
    (lambda (err)
        (funcall out-fn
                 (format nil "FATAL ERROR: ~A~%" err)
        )))


(defun compiler-error (out-fn)
    (lambda (err)
        (funcall out-fn
                 (format nil "COMPILER ERROR: ~A~%" err)
        )))


(defun compiler-note (out-fn)
    (lambda (err)
        (funcall out-fn
                 (format nil "COMPILER NOTE: ~A~%" err)
        )))


(defun handle-error (out-fn)
    (lambda (err)
        (funcall out-fn
                 (format nil "ERROR: ~A~%" err)
        )))


(defun handle-warning (out-fn)
    (lambda (err)
        (funcall out-fn
                 (format nil "WARNING: ~A~%" err)
        )))


(defun file (out path)
    (handler-bind ((sb-c:fatal-compiler-error (compiler-error out))
                   (sb-c:compiler-error (compiler-error out))
                   (sb-ext:compiler-note (compiler-note out))
                   (error (handle-error out))
                   (warning (handle-warning out))
                  )
        (compile-file path)
    ))
