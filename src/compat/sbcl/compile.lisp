(defpackage :alive/compile
    (:use :cl)
    (:export :file)
    (:local-nicknames (:parse :alive/parse/stream))
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


(defun get-form (forms ndx)
    (let ((kids (elt forms 2)))
        (elt kids ndx)
    ))


(defun handle-warning (out-fn path)
    (lambda (err)

        (let* ((context (sb-c::find-error-context nil))
               (source-path (reverse (sb-c::compiler-error-context-original-source-path context)))
              )
            (with-open-file (f path)
                (loop :with forms := (parse:from f)
                      :for ndx :in source-path :do
                          (setf forms (get-form forms ndx))
                      :finally (funcall out-fn (format nil "~A ~A" err forms))
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
