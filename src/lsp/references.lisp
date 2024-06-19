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
    (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls (sym:lookup name pkg-name)))
          ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands (sym:lookup name pkg-name)))))


(declaim (ftype (function (string string) loc:list-of-location) find-references))
(defun find-references (name pkg-name)
    (let ((callers (find-callers name pkg-name)))
        (alive/logger:info-msg "***** FIND REFS ~A IN ~A ~A" name *package* (length callers))
        (loop :for caller :in callers
              :do (alive/logger:info-msg "***** CALLER ~A" caller))))


(declaim (ftype (function (string pos:text-position) loc:list-of-location) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (utils:symbol-for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
