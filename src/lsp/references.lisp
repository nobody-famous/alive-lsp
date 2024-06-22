(defpackage :alive/lsp/references
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:loc :alive/location)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:sym :alive/symbols)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/references)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (cons) (values list &optional)) find-caller-location))
(defun find-caller-location (caller)
    (let ((src (cdr caller)))
        (when src
              (alive/source-utils::get-source-form src)
              (alive/source-utils:get-source-location src))))


(declaim (ftype (function (string string) loc:list-of-location) find-references))
(defun find-references (name pkg-name)
    (let ((callers (find-callers name pkg-name)))
        (remove nil (mapcar #'find-caller-location callers))))


(declaim (ftype (function (string pos:text-position) loc:list-of-location) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (utils:symbol-for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
