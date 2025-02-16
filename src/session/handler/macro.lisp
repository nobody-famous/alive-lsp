(defpackage :alive/session/handler/macro
    (:use :cl)
    (:export :expand
             :expand-1
             :get-text)
    (:local-nicknames (:deps :alive/deps)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/macro)


(declaim (ftype (function (list (function (string string) list)) hash-table) do-expand))
(defun do-expand (msg fn)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (expanded (funcall fn text pkg-name))
           (new-text (if (consp expanded)
                         (princ-to-string expanded)
                         text)))
        (utils:result id "text" (princ-to-string new-text))))


(declaim (ftype (function (list) hash-table) expand))
(defun expand (msg)
    (do-expand msg (lambda (txt pkg)
                       (when (and (stringp txt)
                                  (stringp pkg))
                             (deps:macro-expand txt pkg)))))


(declaim (ftype (function (list) hash-table) expand-1))
(defun expand-1 (msg)
    (do-expand msg (lambda (txt pkg)
                       (when (and (stringp txt)
                                  (stringp pkg))
                             (deps:macro-expand-1 txt pkg)))))


(declaim (ftype (function (hash-table string) string) get-form-text))
(defun get-form-text (form text)
    (when form
          (subseq text
                  (alive/parse/form:get-start-offset form)
                  (alive/parse/form:get-end-offset form))))


(declaim (ftype (function (hash-table string) string) get-macro-name))
(defun get-macro-name (form text)
    (let* ((kids (gethash "kids" form))
           (kid (first kids)))
        (get-form-text kid text)))


(declaim (ftype (function (hash-table string) string) get-defmacro-text))
(defun get-defmacro-text (form text)
    (let* ((kids (gethash "kids" form))
           (name-form (second kids))
           (args-form (third kids))
           (args (mapcar (lambda (f) (get-form-text f text)) (gethash "kids" args-form)))
           (name (get-form-text name-form text)))
        (format nil "(~A ~{~A~^ ~})" name args)))


(declaim (ftype (function (list) hash-table) get-text))
(defun get-text (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (text (cdr (assoc :text params)))
           (forms (alive/parse/forms:from-stream (make-string-input-stream text)))
           (form (first forms))
           (name (get-macro-name form text)))

        (utils:result id "text" (if (string= (string-upcase name) "DEFMACRO")
                                    (get-defmacro-text form text)
                                    text))))
