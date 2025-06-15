(defpackage :alive/sys/xref
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:pos :alive/position)
                      (:sym :alive/symbols)))

(in-package :alive/sys/xref)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (string string) (or null cons)) find-references))
(defun find-references (name pkg-name)
    (mapcar (lambda (caller)
                (list (cons :file (sb-introspect:definition-source-pathname (cdr caller)))
                      (cons :offset (sb-introspect:definition-source-character-offset (cdr caller)))))
            (find-callers name pkg-name)))


(declaim (ftype (function (string pos:text-position) (or null cons)) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
