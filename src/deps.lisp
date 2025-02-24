(defpackage :alive/deps
    (:use :cl)
    (:export :dependencies
             :new-create
             :new-do-compile
             :new-do-eval
             :new-do-load
             :new-get-thread-id
             :new-kill-thread
             :new-list-all-asdf
             :new-list-all-threads
             :new-load-asdf-system
             :new-macro-expand
             :new-macro-expand-1
             :new-msg-handler
             :new-read-msg
             :new-send-msg
             :new-send-request
             :new-try-compile))

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
                                (:load-asdf-system (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) boolean))
                                (:get-thread-id (function (bt:thread) *))
                                (:eval-fn (function (stream) *))
                                (:macro-expand (function (string string) list))
                                (:macro-expand-1 (function (string string) list))
                                (:try-compile (function (string) *))
                                (:do-compile (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *))
                                (:do-load (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *)))
                          dependencies) new-create))
(defun new-create (&key (msg-handler (lambda (deps msg) (declare (ignore deps msg))))
                        (send-msg (lambda (msg) (declare (ignore msg))))
                        (send-request (lambda (req)
                                          (declare (ignore req))
                                          (list)))

                        (read-msg (lambda () (list)))
                        (list-all-threads (lambda () (list)))
                        (kill-thread (lambda (id) (declare (ignore id))))
                        (list-all-asdf (lambda () (list)))
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
                       :load-asdf-system load-asdf-system
                       :get-thread-id get-thread-id
                       :eval-fn eval-fn
                       :macro-expand macro-expand
                       :macro-expand-1 macro-expand-1
                       :try-compile try-compile
                       :do-compile do-compile
                       :do-load do-load))


(declaim (ftype (function (dependencies) T) new-msg-handler))
(defun new-msg-handler (deps)
    (dependencies-msg-handler deps))


(declaim (ftype (function (dependencies) T) new-read-msg))
(defun new-read-msg (deps)
    (funcall (dependencies-read-msg deps)))


(declaim (ftype (function (dependencies T) (values null &optional)) new-send-msg))
(defun new-send-msg (deps msg)
    (funcall (dependencies-send-msg deps) msg))


(declaim (ftype (function (dependencies hash-table) (values list &optional)) new-send-request))
(defun new-send-request (deps msg)
    (funcall (dependencies-send-request deps) msg))


(declaim (ftype (function (dependencies) (values cons &optional)) new-list-all-threads))
(defun new-list-all-threads (deps)
    (funcall (dependencies-list-all-threads deps)))


(declaim (ftype (function (dependencies T) *) new-kill-thread))
(defun new-kill-thread (deps thread-id)
    (funcall (dependencies-kill-thread deps) thread-id))


(declaim (ftype (function (dependencies bt:thread) *) new-get-thread-id))
(defun new-get-thread-id (deps thread)
    (funcall (dependencies-get-thread-id deps) thread))


(declaim (ftype (function (dependencies) (values cons &optional)) new-list-all-asdf))
(defun new-list-all-asdf (deps)
    (funcall (dependencies-list-all-asdf deps)))


(declaim (ftype (function (dependencies &key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) (values boolean &optional)) new-load-asdf-system))
(defun new-load-asdf-system (deps &key name stdin-fn stdout-fn stderr-fn force)
    (funcall (dependencies-load-asdf-system deps)
        :name name
        :stdin-fn stdin-fn
        :stdout-fn stdout-fn
        :stderr-fn stderr-fn
        :force force))


(declaim (ftype (function (dependencies T) *) new-do-eval))
(defun new-do-eval (deps data)
    (let ((results (multiple-value-list (funcall (dependencies-eval-fn deps) data))))
        (finish-output)
        results))


(declaim (ftype (function (dependencies string string) (values list &optional)) new-macro-expand))
(defun new-macro-expand (deps txt pkg)
    (funcall (dependencies-macro-expand deps) txt pkg))


(declaim (ftype (function (dependencies string string) (values list &optional)) new-macro-expand-1))
(defun new-macro-expand-1 (deps txt pkg)
    (funcall (dependencies-macro-expand-1 deps) txt pkg))


(declaim (ftype (function (dependencies string) *) new-try-compile))
(defun new-try-compile (deps path)
    (funcall (dependencies-try-compile deps) path))


(declaim (ftype (function (dependencies string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) new-do-compile))
(defun new-do-compile (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-compile deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(declaim (ftype (function (dependencies string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) new-do-load))
(defun new-do-load (deps path &key stdin-fn stdout-fn stderr-fn)
    (funcall (dependencies-do-load deps) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))
