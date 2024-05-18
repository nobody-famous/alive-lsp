(defpackage :alive/session/handler/form-bounds
    (:use :cl)
    (:export :surrounding-form
             :top-form)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/form-bounds)


(declaim (ftype (function (cons) (or null cons)) get-forms))
(defun get-forms (msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text uri) "")))

        (forms:from-stream-or-nil (make-string-input-stream text))))


(declaim (ftype (function (fixnum (or null alive/position:text-position) (or null alive/position:text-position)) hash-table) create-response))
(defun create-response (id start end)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "start" data) start)
        (setf (gethash "end" data) end)

        (lsp-msg:create-response id
                                 :result-value data)))


(declaim (ftype (function (cons) hash-table) top-form))
(defun top-form (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pos (cdr (assoc :position params)))
           (forms (get-forms msg))
           (form (forms:get-top-form forms pos))
           (start (when form (gethash "start" form)))
           (end (when form (gethash "end" form))))
        (create-response id start end)))


(declaim (ftype (function (cons) hash-table) surrounding-form))
(defun surrounding-form (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pos (cdr (assoc :position params)))
           (forms (get-forms msg))
           (top-form (forms:get-top-form forms pos))
           (form (forms:get-outer-form top-form pos))
           (start (when form (gethash "start" form)))
           (end (when form (gethash "end" form))))
        (create-response id start end)))
