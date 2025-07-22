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
             :try-compile))

(in-package :alive/deps)


(defstruct dependencies
    (msg-handler nil :type (or null (function (dependencies cons) (values (or null hash-table) &optional))))
    (read-msg nil :type (or null (function () (values (or null cons) &optional))))
    (send-msg nil :type (or null (function (cons) null)))
    (send-request nil :type (or null (function (list) hash-table)))
    (eval-fn nil :type (or null (function (T) *)))
    (list-all-threads nil :type (or null (function () cons)))
    (kill-thread nil :type (or null (function (T) *)))
    (list-all-asdf nil :type (or null (function () cons)))
    (list-all-traced nil :type (or null (function () (or null cons))))
    (trace-fn nil :type (or null (function (string string) *)))
    (load-asdf-system nil :type (or null (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) boolean)))
    (get-thread-id nil :type (or null (function (bt:thread) *)))
    (macro-expand nil :type (or null (function (string string) list)))
    (macro-expand-1 nil :type (or null (function (string string) list)))
    (try-compile nil :type (or null (function (string) *)))
    (do-compile nil :type (or null (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *)))
    (do-load nil :type (or null (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *))))


(declaim (ftype (function (&key (:msg-handler (function (dependencies cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:send-request (function (hash-table) list))
                                (:read-msg (function () (values (or null cons) &optional)))
                                (:list-all-threads (function () cons))
                                (:kill-thread (function (T) *))
                                (:list-all-asdf (function () cons))
                                (:list-all-traced (function () (or null cons)))
                                (:trace-fn (function (string string) *))
                                (:load-asdf-system (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) boolean))
                                (:get-thread-id (function (bt:thread) *))
                                (:eval-fn (function (stream) *))
                                (:macro-expand (function (string string) list))
                                (:macro-expand-1 (function (string string) list))
                                (:try-compile (function (string) *))
                                (:do-compile (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *))
                                (:do-load (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *)))
                          dependencies) create))
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
                    (trace-fn (lambda (pkg-name fn-name) (declare (ignore pkg-name fn-name)) nil))
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
                       :load-asdf-system load-asdf-system
                       :get-thread-id get-thread-id
                       :eval-fn eval-fn
                       :macro-expand macro-expand
                       :macro-expand-1 macro-expand-1
                       :try-compile try-compile
                       :do-compile do-compile
                       :do-load do-load))


(declaim (ftype (function (dependencies) T) msg-handler))
(defun msg-handler (deps)
    (dependencies-msg-handler deps))


(declaim (ftype (function (dependencies) T) read-msg))
(defun read-msg (deps)
    (funcall (dependencies-read-msg deps)))


(declaim (ftype (function (dependencies T) (values null &optional)) send-msg))
(defun send-msg (deps msg)
    (funcall (dependencies-send-msg deps) msg))


(declaim (ftype (function (dependencies hash-table) (values list &optional)) send-request))
(defun send-request (deps msg)
    (funcall (dependencies-send-request deps) msg))


(declaim (ftype (function (dependencies) (values cons &optional)) list-all-threads))
(defun list-all-threads (deps)
    (funcall (dependencies-list-all-threads deps)))


(declaim (ftype (function (dependencies T) *) kill-thread))
(defun kill-thread (deps thread-id)
    (funcall (dependencies-kill-thread deps) thread-id))


(declaim (ftype (function (dependencies bt:thread) *) get-thread-id))
(defun get-thread-id (deps thread)
    (funcall (dependencies-get-thread-id deps) thread))


(declaim (ftype (function (dependencies) (values cons &optional)) list-all-asdf))
(defun list-all-asdf (deps)
    (funcall (dependencies-list-all-asdf deps)))


(declaim (ftype (function (dependencies) (values (or null cons) &optional)) list-all-traced))
(defun list-all-traced (deps)
    (funcall (dependencies-list-all-traced deps)))


(declaim (ftype (function (dependencies string string) *) trace-fn))
(defun trace-fn (deps pkg-name fn-name)
    (funcall (dependencies-trace-fn deps) pkg-name fn-name))


(declaim (ftype (function (dependencies &key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) (values boolean &optional)) load-asdf-system))
(defun load-asdf-system (deps &key name stdin-fn stdout-fn stderr-fn force)
    (funcall (dependencies-load-asdf-system deps)
        :name name
        :stdin-fn stdin-fn
        :stdout-fn stdout-fn
        :stderr-fn stderr-fn
        :force force))


(declaim (ftype (function (dependencies T) *) do-eval))
(defun do-eval (deps data)
    (let ((results (multiple-value-list (funcall (dependencies-eval-fn deps) data))))
        (finish-output)
        results))


(declaim (ftype (function (dependencies string string) (values list &optional)) macro-expand))
(defun macro-expand (deps txt pkg)
    (funcall (dependencies-macro-expand deps) txt pkg))


(declaim (ftype (function (dependencies string string) (values list &optional)) macro-expand-1))
(defun macro-expand-1 (deps txt pkg)
    (funcall (dependencies-macro-expand-1 deps) txt pkg))


(declaim (ftype (function (dependencies string) *) try-compile))
(defun try-compile (deps path)
    (funcall (dependencies-try-compile deps) path))


(declaim (ftype (function (dependencies string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-compile))
(defun do-compile (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-compile deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(declaim (ftype (function (dependencies string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-load))
(defun do-load (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-load deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))
