(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:types :alive/types)))

(in-package :alive/sbcl/file)


(defun get-form (forms ndx)
    (let ((kids (elt forms 2)))
        (elt kids ndx)))


(defun get-err-location (forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (reverse (sb-c::compiler-error-context-original-source-path context))))
        (loop :for ndx :in source-path :do
                  (setf forms (get-form forms ndx))
              :finally (return (subseq forms 0 2)))))


(defun send-message (out-fn forms sev err)
    (let* ((loc (get-err-location forms))
           (msg (types:make-compile-message :severity sev
                                            :location loc
                                            :message (format nil "~A" err))))
        (funcall out-fn msg)))


(defun fatal-error (out-fn forms)
    (lambda (err)
        (send-message out-fn forms types:*sev-error* err)))


(defun compiler-error (out-fn forms)
    (lambda (err)
        (send-message out-fn forms types:*sev-error* err)))


(defun compiler-note (out-fn forms)
    (lambda (err)
        (send-message out-fn forms types:*sev-info* err)))


(defun handle-error (out-fn forms)
    (lambda (err)
        (send-message out-fn forms types:*sev-error* err)))


(defun handle-warning (out-fn forms)
    (lambda (err)
        (send-message out-fn forms types:*sev-warn* err)))


(defun do-cmd (path out cmd)
    (with-open-file (f path)
        (let ((forms (parse:from f)))
            (handler-bind ((sb-c:fatal-compiler-error (fatal-error out forms))
                           (sb-c:compiler-error (compiler-error out forms))
                           (sb-ext:compiler-note (compiler-note out forms))
                           (error (handle-error out forms))
                           (warning (handle-warning out forms)))
                (funcall cmd path)))))


(defun do-compile (path out)
    (with-open-file (f path)
        (let ((forms (parse:from f)))
            (handler-bind ((sb-c:fatal-compiler-error (fatal-error out forms))
                           (sb-c:compiler-error (compiler-error out forms))
                           (sb-ext:compiler-note (compiler-note out forms))
                           (error (handle-error out forms))
                           (warning (handle-warning out forms)))
                (compile-file path)))))


(defun do-load (path out-fn)
    (do-cmd path out-fn 'load))
