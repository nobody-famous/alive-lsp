(defpackage :alive/sys/eval
    (:use :cl)
    (:export :eval-fn
             :from-string)
    (:local-nicknames (:astreams :alive/sys/streams)
                      (:deps :alive/deps)
                      (:pkgs :alive/packages)))

(in-package :alive/sys/eval)


(declaim (ftype (function (stream) *) eval-fn))
(defun eval-fn (input)
    (eval (read input)))


(declaim (ftype (function (deps:dependencies string &key
                                             (:pkg-name string)
                                             (:stdin-fn function)
                                             (:stdout-fn function)
                                             (:stderr-fn function)
                                             (:query-fn function)
                                             (:trace-fn function)) *) from-string))
(defun from-string (deps str &key pkg-name stdin-fn stdout-fn stderr-fn query-fn trace-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn :query-fn query-fn :trace-fn trace-fn)
        (let* ((input (make-string-input-stream str))
               (pkg (pkgs:lookup pkg-name))
               (*package* (if pkg pkg *package*)))
            (when (and pkg-name (not pkg))
                  (error (make-condition 'pkgs:package-not-found :name pkg-name)))

            (deps:do-eval deps input))))
