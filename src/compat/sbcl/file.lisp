(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:parse :alive/parse/stream)
                      (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:token :alive/parse/token)
                      (:errors :alive/errors)
                      (:range :alive/range)
                      (:pos :alive/position)
                      (:types :alive/types)
                      (:comp-msg :alive/compile-message)))

(in-package :alive/sbcl/file)


(defun get-err-location (forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (when context (reverse (sb-c::compiler-error-context-original-source-path context)))))

        (if (not source-path)
            (range:create (form:get-start (car forms))
                          (form:get-end (car (reverse forms))))

            (forms:get-range-for-path forms source-path))))


(defun send-message (out-fn forms sev err)
    (let* ((loc (get-err-location forms))
           (msg (comp-msg:create :severity sev
                                 :location loc
                                 :message (format nil "~A" err))))

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
        (let ((forms (forms:from-stream f)))
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


(defun already-have-msg-p (new-msg msgs)
    (find-if (lambda (msg)
                 (and (string= (gethash "severity" new-msg)
                               (gethash "severity" msg))
                      (string= (gethash "message" new-msg)
                               (gethash "message" msg))))
            msgs))


(defun should-filter-p (msg)
    (search "redefining" (comp-msg:get-message msg)))


(defun try-compile (path)
    (with-open-file (f path)
        (let ((msgs nil))

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

            (handler-bind ((T (lambda (e)
                                  (format T "~A~%" (type-of e))
                                  (loop :for item :in (compute-restarts e)
                                        :do (format T "  ~A~%" (restart-name item)))
                                  (let ((skip (or (find-restart 'muffle-warning e)
                                                  (find-restart 'continue e))))
                                      (when skip
                                            (invoke-restart skip))))))
                (do-cmd path 'compile-file
                        (lambda (msg)
                            (unless (or (already-have-msg-p msg msgs)
                                        (should-filter-p msg))
                                (setf msgs (cons msg msgs)))))
                msgs))))