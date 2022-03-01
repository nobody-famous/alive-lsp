(defpackage :alive/sbcl/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
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

        (when source-path
              (loop :for ndx :in source-path :do
                        (setf forms (get-form forms ndx))
                    :finally (return (subseq forms 0 2))))))


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
        (let ((forms (parse:from f))
              (msgs nil))

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
            ;; The only issue I've seen so far is that defpackage forms will update the
            ;; packages, which results in annoying "package also exports" warnings.
            ;;  1. parse the file before compiling, get a list of packages, and drop them
            ;;  2. get the list of packages, along with their aliases, rename them to temp names,
            ;;     delete the version compile creates, and rename the temp ones back to their
            ;;     original names. This seems like the most feasible option right now.
            ;;

            (handler-case

                    (do-cmd path 'compile-file
                            (lambda (msg)
                                (setf msgs (cons msg msgs))))

                (error (e)
                       (send-message (lambda (msg)
                                         (setf msgs (cons msg msgs)))
                                     forms
                                     types:*sev-error*
                                     e)))

            msgs)))
