(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:types :alive/types)
                      (:comp-msg :alive/compile-message)))

(in-package :alive/sbcl/file)


(defun get-form (forms ndx)
    (let ((kids (elt forms 2)))
        (elt kids ndx)))


(defun get-err-location (forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (when context (reverse (sb-c::compiler-error-context-original-source-path context)))))

        (loop :for ndx :in source-path :do
                  (setf forms (get-form forms ndx))
              :finally (return (subseq forms 0 2)))))


(defun send-message (out-fn forms sev err)
    (let* ((loc (get-err-location forms))
           (msg (comp-msg:create :severity sev
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


; (defun do-cmd (path cmd out)
;     (with-open-file (f path)
;         (let ((forms (parse:from f)))
;             (handler-case

;                     (funcall cmd path)

;                 (sb-c:fatal-compiler-error (e)
;                                            (send-message out forms types:*sev-error* e))
;                 (sb-c:compiler-error (e)
;                                      (send-message out forms types:*sev-error* e))
;                 (sb-ext:compiler-note (e)
;                                       (send-message out forms types:*sev-info* e))
;                 (error (e)
;                        (send-message out forms types:*sev-error* e))
;                 (warning (e)
;                          (format T "GOT WARNING ~A~%" e)
;                          (send-message out forms types:*sev-warn* e))))))



(defun do-cmd (path cmd out)
    (with-open-file (f path)
        (let ((forms (parse:from f)))
            (handler-bind ((sb-c:fatal-compiler-error (fatal-error out forms))
                           (sb-c:compiler-error (compiler-error out forms))
                           (sb-ext:compiler-note (compiler-note out forms))
                           (sb-ext::simple-style-warning (handle-warning out forms))
                           (error (handle-error out forms))
                           (warning (handle-warning out forms)))
                (funcall cmd path)))))


(defun do-compile (path)
    (let ((msgs nil))
        (do-cmd path 'compile-file
                (lambda (msg)
                    (setf msgs (cons msg msgs))))
        msgs))


(defun do-load (path)
    (let ((msgs nil))
        (do-cmd path 'compile-file
                (lambda (msg)
                    (setf msgs (cons msg msgs))))

        (with-open-file (f path)
            (let ((forms (parse:from f)))
                (handler-case

                        (load path)

                    (error (e)
                           (send-message (lambda (msg)
                                             (setf msgs (cons msg msgs)))
                                         forms
                                         types:*sev-error*
                                         e)))))

        msgs))
