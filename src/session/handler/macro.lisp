(defpackage :alive/session/handler/macro
    (:use :cl)
    (:export :new-expand
             :new-expand-1)
    (:local-nicknames (:deps :alive/deps)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/macro)


(declaim (ftype (function (list (function ((or null string) (or null string)) list)) hash-table) do-expand))
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


(declaim (ftype (function (deps:dependencies list) hash-table) new-expand))
(defun new-expand (deps msg)
    (do-expand msg (lambda (txt pkg)
                       (when (and (stringp txt)
                                  (stringp pkg))
                             (deps:new-macro-expand deps txt pkg)))))


(declaim (ftype (function (deps:dependencies list) hash-table) new-expand-1))
(defun new-expand-1 (deps msg)
    (do-expand msg (lambda (txt pkg)
                       (when (and (stringp txt)
                                  (stringp pkg))
                             (deps:new-macro-expand-1 deps txt pkg)))))
