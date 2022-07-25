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
                      (:types :alive/types)
                      (:comp-msg :alive/compile-message)))

(in-package :alive/sbcl/file)


(defun should-skip (form-type)
    (and form-type
         (or (= types:*ifdef-false* form-type)
             (= types:*line-comment* form-type)
             (= types:*block-comment* form-type))))


(defun get-nth-form (forms offset)
    (loop :with counted := 0
          :with cur-form := nil

          :while (<= counted offset)

          :do (setf cur-form (pop forms))

              (cond ((or (= types:*line-comment* (form:get-form-type cur-form))
                         (= types:*block-comment* (form:get-form-type cur-form)))
                        NIL)

                    ((= types:*ifdef-false* (form:get-form-type cur-form))
                        (pop forms))

                    (T (incf counted)))

          :finally (return cur-form)))


(defun get-err-location (forms)
    (let* ((context (sb-c::find-error-context nil))
           (source-path (when context (reverse (sb-c::compiler-error-context-original-source-path context)))))

        (if (not source-path)
            (range:create (form:get-start-new (car forms))
                          (form:get-end-new (car (reverse forms))))

            (loop :with indicies := source-path
                  :with ndx := nil
                  :with form := nil

                  :while indicies
                  :do (setf ndx (pop indicies))

                      (when (<= (length forms) ndx)
                            (error (format nil "Source ndx ~A, path ~A, form ~A" ndx source-path form)))

                      (setf form (get-nth-form forms ndx))

                      (setf forms (form:get-kids-new form))

                  :finally (return (range:create (form:get-start-new form)
                                                 (form:get-end-new form)))))))


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
        (let ((forms (forms:from-stream-new f)))
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


(defun filter-warnings (msgs)
    (remove-if (lambda (msg)
                   (search "redefining" (comp-msg:message msg)))
            msgs))


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

            (handler-case
                    (progn (do-cmd path 'compile-file
                                   (lambda (msg)
                                       (setf msgs (cons msg msgs))))

                           msgs)
                (errors:input-error (e)
                                    (push (comp-msg:create :severity types:*sev-error*
                                                           :location (range:create (errors:start e) (errors:end e))
                                                           :message (format nil "~A" e))
                                          msgs)
                                    msgs)
                (sb-ext:package-does-not-exist (e)
                                               (declare (ignore e))
                                               msgs)

                (sb-c::simple-package-error (e)
                                            (declare (ignore e))
                                            msgs)

                (T (e)
                   (declare (ignore e))
                   (filter-warnings msgs))))))