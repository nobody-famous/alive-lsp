(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:forms :alive/parse/forms)
                      (:errors :alive/errors)
                      (:types :alive/types)
                      (:comp-msg :alive/compile-message)))

(in-package :alive/sbcl/file)


(defun get-form (forms ndx)
    (let ((kids (elt forms 2)))
        (elt kids ndx)))


(defun get-err-location (forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (when context (reverse (sb-c::compiler-error-context-original-source-path context)))))

        (format T "~&source-path ~A~%" source-path)
        (when source-path
              (loop :for ndx :in source-path :do
                        (setf forms (get-form forms ndx))
                    :finally (return (subseq forms 0 2))))))


(defun send-message (out-fn forms sev err)
    (let* ((loc (get-err-location forms))
           (msg (comp-msg:create :severity sev
                                 :location loc
                                 :message (format nil "~A" err))))

        (format T "send-message ~A ~A~%" loc err)
        (when loc
              (funcall out-fn msg))))


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

        (do-cmd path 'load
                (lambda (msg)
                    (setf msgs (cons msg msgs))))

        msgs))


(defun try-compile (path)
    (with-open-file (f path)
        (let ((msgs nil))

            (handler-case

                    ;;
                ;; Compiling a file corrupts the environment. The goal here is to compile without
                ;; that happening and just get a list of compiler messages for the file.
                ;;
                ;; One idea was to fork and have the child process do the compile. That would keep
                ;; the parent from getting corrupted. That approach hit some problems.
                ;;   1. sbcl won't fork if there's multiple threads active
                ;;   2. sbcl only implements fork for unix, anyway, so wouldn't work on Windows
                ;;
                ;; One possible solution to the fork issue would be to use FFI to call the C fork
                ;; function directly. That seems problematic.
                ;;
                ;; The main issue is that def* calls will update the environment. There may be some
                ;; way to account for that. I haven't figured it out, yet.
                ;;

                (progn (do-cmd path 'compile-file
                               (lambda (msg)
                                   (setf msgs (cons msg msgs))))

                       msgs)
                (errors:input-error (e)
                                    (setf msgs (cons
                                                (comp-msg:create :severity types:*sev-error*
                                                                 :location (list (errors:start e) (errors:end e))
                                                                 :message (format nil "~A" e))
                                                msgs)))))))