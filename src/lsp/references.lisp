(defpackage :alive/lsp/references
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:loc :alive/location)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:sym :alive/symbols)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/references)


(defun setup-readtable (source-map)
    (let* ((rt (copy-readtable *readtable*))
           (orig-fn (get-dispatch-macro-character #\# #\. rt)))
        (set-dispatch-macro-character #\# #\.
                                      (lambda (input char n)
                                          (format T "START ~A~%" (file-position input))
                                          (ignore-errors (funcall orig-fn input char n))
                                          (format T "END ~A~%" (file-position input)))
                                      rt)
        (loop :for ch :from 0 :to 128
              :do (multiple-value-bind (fun non-terminal)
                          (get-macro-character (code-char ch) rt)
                      (when fun
                            (set-macro-character (code-char ch)
                                                 (lambda (input char)
                                                     (format T "MACRO CHARACTER ~A ~A~%" char (file-position input))
                                                     (let ((start (1- (file-position input)))
                                                           (values (multiple-value-list (funcall fun input char)))
                                                           (end (file-position input)))
                                                         (push (cons start end) (gethash (car values) source-map))
                                                         (format T "VALUES ~A ~A ~A ~A~%" start end values (values-list values))
                                                         (values-list values)))
                                                 non-terminal
                                                 rt))))
        rt))


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
              (alive/source-utils:get-source-form src))))


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
