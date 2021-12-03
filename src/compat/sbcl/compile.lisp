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


(defun handle-error (err)
    (format nil "ERROR: ~A~%" err)
)


(defun handle-warning (out-fn path)
    (lambda (err)
        (let* ((context (sb-c::find-error-context nil))
               (source-path (reverse (sb-c::compiler-error-context-original-source-path context)))
              )
            (with-open-file (f path)
                (let ((form (read f)))
                    (funcall out-fn
                             (format nil "FORM ~A ~A~%" (elt form (second source-path)) form)
                    )))

            (funcall out-fn
                     (format nil "WARNING: ~A~%~A~%~A~%"
                             (sb-c::compiler-error-context-file-name context)
                             (reverse (sb-c::compiler-error-context-original-source-path context))
                             err
                     )))))


(defun file (out path)
    (handler-bind ((sb-c:fatal-compiler-error (compiler-error out))
                   (sb-c:compiler-error (compiler-error out))
                   (sb-ext:compiler-note (compiler-note out))
                   (error #'handle-error)
                   (warning (handle-warning out path))
                  )
        (compile-file path)
    ))
