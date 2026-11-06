(defpackage :alive/deps
    (:use :cl)
    (:export :dependencies
             :create
             :do-compile
             :do-eval
             :do-load
             :get-thread-id
             :kill-thread
             :list-all-asdf
             :list-all-threads
             :list-all-traced
             :load-asdf-system
             :macro-expand
             :macro-expand-1
             :msg-handler
             :read-msg
             :send-msg
             :send-request
             :trace-fn
             :trace-pkg
             :try-compile
             :untrace-fn
             :untrace-pkg))

(in-package :alive/deps)


(defstruct dependencies
    (msg-handler nil)
    (read-msg nil)
    (send-msg nil)
    (send-request nil)
    (eval-fn nil)
    (list-all-threads nil)
    (kill-thread nil)
    (list-all-asdf nil)
    (list-all-traced nil)
    (trace-fn nil)
    (untrace-fn nil)
    (trace-pkg nil)
    (untrace-pkg nil)
    (load-asdf-system nil)
    (get-thread-id nil)
    (macro-expand nil)
    (macro-expand-1 nil)
    (try-compile nil)
    (do-compile nil)
    (do-load nil))


(defun create (&key (msg-handler (lambda (deps msg) (declare (ignore deps msg))))
                    (send-msg (lambda (msg) (declare (ignore msg))))
                    (send-request (lambda (req)
                                      (declare (ignore req))
                                      (list)))

                    (read-msg (lambda () (list)))
                    (list-all-threads (lambda () (list)))
                    (kill-thread (lambda (id) (declare (ignore id))))
                    (list-all-asdf (lambda () (list)))
                    (list-all-traced (lambda () (list)))
                    (trace-fn (lambda (fn-name) (declare (ignore fn-name)) nil))
                    (untrace-fn (lambda (fn-name) (declare (ignore fn-name)) nil))
                    (trace-pkg (lambda (pkg-name) (declare (ignore pkg-name)) nil))
                    (untrace-pkg (lambda (pkg-name) (declare (ignore pkg-name)) nil))
                    (load-asdf-system (lambda (&key name stdin-fn stdout-fn stderr-fn force)
                                          (declare (ignore name stdin-fn stdout-fn stderr-fn force))
                                          T))
                    (get-thread-id (lambda (thread) (declare (ignore thread))))
                    (eval-fn (lambda (s) (declare (ignore s))))
                    (macro-expand (lambda (txt pkg)
                                      (declare (ignore txt pkg)
                                               (list))))
                    (macro-expand-1 (lambda (txt pkg)
                                        (declare (ignore txt pkg)
                                                 (list))))
                    (try-compile (lambda (path) (declare (ignore path))))
                    (do-compile (lambda (path &key stdin-fn stdout-fn stderr-fn)
                                    (declare (ignore path stdin-fn stdout-fn stderr-fn))))
                    (do-load (lambda (path &key stdin-fn stdout-fn stderr-fn)
                                 (declare (ignore path stdin-fn stdout-fn stderr-fn)))))
    (make-dependencies :msg-handler msg-handler
                       :send-msg send-msg
                       :send-request send-request
                       :read-msg read-msg
                       :list-all-threads list-all-threads
                       :kill-thread kill-thread
                       :list-all-asdf list-all-asdf
                       :list-all-traced list-all-traced
                       :trace-fn trace-fn
                       :untrace-fn untrace-fn
                       :trace-pkg trace-pkg
                       :untrace-pkg untrace-pkg
                       :load-asdf-system load-asdf-system
                       :get-thread-id get-thread-id
                       :eval-fn eval-fn
                       :macro-expand macro-expand
                       :macro-expand-1 macro-expand-1
                       :try-compile try-compile
                       :do-compile do-compile
                       :do-load do-load))


(defun msg-handler (deps)
    (dependencies-msg-handler deps))


(defun read-msg (deps)
    (funcall (dependencies-read-msg deps)))


(defun send-msg (deps msg)
    (funcall (dependencies-send-msg deps) msg))


(defun send-request (deps msg)
    (funcall (dependencies-send-request deps) msg))


(defun list-all-threads (deps)
    (funcall (dependencies-list-all-threads deps)))


(defun kill-thread (deps thread-id)
    (funcall (dependencies-kill-thread deps) thread-id))


(defun get-thread-id (deps thread)
    (funcall (dependencies-get-thread-id deps) thread))


(defun list-all-asdf (deps)
    (funcall (dependencies-list-all-asdf deps)))


(defun list-all-traced (deps)
    (funcall (dependencies-list-all-traced deps)))


(defun trace-fn (deps fn-name)
    (funcall (dependencies-trace-fn deps) fn-name))


(defun untrace-fn (deps fn-name)
    (funcall (dependencies-untrace-fn deps) fn-name))


(defun trace-pkg (deps pkg-name)
    (funcall (dependencies-trace-pkg deps) pkg-name))


(defun untrace-pkg (deps pkg-name)
    (funcall (dependencies-untrace-pkg deps) pkg-name))


(defun load-asdf-system (deps &key name stdin-fn stdout-fn stderr-fn force)
    (funcall (dependencies-load-asdf-system deps)
        :name name
        :stdin-fn stdin-fn
        :stdout-fn stdout-fn
        :stderr-fn stderr-fn
        :force force))


(defun do-eval (deps data)
    (let ((results (multiple-value-list (funcall (dependencies-eval-fn deps) data))))
        (finish-output)
        results))


(defun macro-expand (deps txt pkg)
    (funcall (dependencies-macro-expand deps) txt pkg))


(defun macro-expand-1 (deps txt pkg)
    (funcall (dependencies-macro-expand-1 deps) txt pkg))


(defun try-compile (deps path)
    (funcall (dependencies-try-compile deps) path))


(defun do-compile (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-compile deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(defun do-load (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-load deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))
