(defpackage :alive/sys/xref
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:pos :alive/position)
                      (:sym :alive/symbols)
                      (:utils :alive/lsp/utils)))

(in-package :alive/sys/xref)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (cons) (values list &optional)) find-caller-location))
(defun find-caller-location (caller)
    (declare (ignore caller))
    #+n (let ((src (cdr caller)))
            (when src
                  (alive/source-utils:get-source-form src))))


#+n (declaim (ftype (function (string string) loc:list-of-location) find-references))
(defun find-references (name pkg-name)
    (format T "***** FIND REFS ~A ~A ~A~%" name pkg-name (find-callers name pkg-name))
    #+n (let ((callers (find-callers name pkg-name)))
            (remove nil (mapcar #'find-caller-location callers))))


#+n (declaim (ftype (function (string pos:text-position) loc:list-of-location) get-locations))
(declaim (ftype (function (string pos:text-position) *) get-locations))
(defun get-locations (text pos)
    #+n (declare (ignore text pos))
    (multiple-value-bind (name pkg-name)
            (utils:symbol-for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
