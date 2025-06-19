(defpackage :alive/sys/xref
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:pos :alive/position)
                      (:sym :alive/symbols)
                      (:utils :alive/utils)))

(in-package :alive/sys/xref)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (cons) (or null cons)) caller-to-location))
(defun caller-to-location (caller)
    (let* ((path (sb-introspect:definition-source-pathname (cdr caller)))
           (path-str (if path
                         (namestring (sb-introspect:definition-source-pathname (cdr caller)))
                         nil))
           (file (if path-str
                     (utils:url-encode-filename path-str)
                     nil)))
        (list (cons :file file)
              (cons :offset (sb-introspect:definition-source-character-offset (cdr caller))))))


(declaim (ftype (function (string string) (or null cons)) find-references))
(defun find-references (name pkg-name)
    (let* ((locations (mapcar #'caller-to-location (find-callers name pkg-name))))
        (remove-if-not (lambda (caller)
                           (stringp (cdr (assoc :file caller))))
                locations)))


(declaim (ftype (function (string pos:text-position) (or null cons)) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
