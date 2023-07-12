(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:logger :alive/logger)
                      (:token :alive/parse/token)
                      (:errors :alive/errors)
                      (:range :alive/range)
                      (:pos :alive/position)
                      (:types :alive/types)
                      (:comp-msg :alive/compile-message)))

(in-package :alive/sbcl/file)


(defmacro with-forms ((path) &body body)
    (let ((file-id (gensym)))
        `(with-open-file (,file-id ,path)
             (let ((forms (forms:from-stream ,file-id)))
                 ,@body))))


(defun get-int-value (needle hay start delim)
    (let* ((pos (search needle hay :start2 start))
           (begin (when pos (+ pos (length needle))))
           (end (when begin (search delim hay :start2 begin))))

        (when (and begin end)
              (parse-integer (subseq hay begin end)))))


(defun parse-err-loc (err-msg)
    (let ((line (get-int-value "line: " err-msg 0 ","))
          (col (get-int-value "column: " err-msg 0 ",")))

        (when (and line col)
              (range:create (pos:create (- line 1) col)
                            (pos:create (- line 1) #xFFFF)))))


(defun get-err-location (err forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (when context (reverse (sb-c::compiler-error-context-original-source-path context)))))

        (if (not source-path)
            (parse-err-loc (string-downcase (princ-to-string err)))
            (forms:get-range-for-path forms source-path))))


(defun send-message (out-fn forms sev err)
    (let* ((loc (get-err-location err forms))
           (msg (comp-msg:create :severity sev
                                 :location loc
                                 :message (format nil "~A" err))))

        (when loc
              (funcall out-fn msg))))


(defun already-have-msg-p (new-msg msgs)
    (find-if (lambda (msg)
                 (and (string= (gethash "severity" new-msg)
                               (gethash "severity" msg))
                      (string= (gethash "message" new-msg)
                               (gethash "message" msg))))
            msgs))


(defun should-filter-p (msg)
    (search "redefin" msg))


(defun add-message (msgs msg)
    (if (or (already-have-msg-p msg msgs)
            (should-filter-p (comp-msg:get-message msg)))
        msgs
        (cons msg msgs)))


(defun do-cmd (path cmd &optional (stop-on-error nil))
    (with-forms (path)
        (let* ((msgs nil)
               (capture-msg (lambda (msg)
                                (setf msgs (add-message msgs msg))))
               (handle-error (lambda (err)
                                 (send-message capture-msg forms types:*sev-error* err)
                                 (when (and (not (should-filter-p (format NIL "~A" err)))
                                            stop-on-error)
                                       (return-from do-cmd msgs))))
               (handle-defconstant (lambda (err)
                                       (when stop-on-error
                                             ; Redefining a constant is an error, not a warning. If we ignore it, it'll
                                             ; trigger the debugger. One of the restarts is to keep the old value, so
                                             ; find and invoke it.
                                             (progn (loop :for item :in (compute-restarts err)
                                                          :do (when (search "old value" (format nil "~A" item))
                                                                    (invoke-restart item)))
                                                    ; Didn't find the restart, so just bail
                                                    (return-from do-cmd msgs))))))
            (labels ((handle-skippable (sev)
                                       (lambda (err)
                                           (send-message capture-msg forms sev err)
                                           (let ((skip (find-restart 'muffle-warning err)))
                                               (if skip
                                                   (invoke-restart skip)
                                                   (return-from do-cmd msgs))))))
                (handler-bind ((sb-ext::simple-style-warning (handle-skippable types:*sev-warn*))
                               (sb-ext:compiler-note (handle-skippable types:*sev-info*))
                               (sb-ext:defconstant-uneql handle-defconstant)
                               (warning (handle-skippable types:*sev-warn*))
                               (sb-c:fatal-compiler-error handle-error)
                               (sb-c:compiler-error handle-error)
                               (error handle-error))
                    (funcall cmd path)
                    msgs)))))


(defun do-compile (path)
    (do-cmd path 'compile-file))

(defun do-load (path)
    (do-compile path)
    (do-cmd path 'load))

(defun try-compile (path)
    (do-cmd path 'compile-file T))
