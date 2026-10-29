(defpackage :alive/session/handler/form-bounds
    (:use :cl)
    (:export :surrounding-form
             :top-form)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/form-bounds)


(defun create-response (id start end)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "start" data) start)
        (setf (gethash "end" data) end)

        (lsp-msg:create-response id
                                 :result-value data)))


(defun top-form (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pos (cdr (assoc :position params)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (forms (state:get-file-forms state uri))
           (form (forms:get-top-form forms pos))
           (start (when form (form:xyz-get-start form)))
           (end (when form (form:get-end form))))
        (create-response id start end)))


(defun surrounding-form (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pos (cdr (assoc :position params)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (forms (state:get-file-forms state uri))
           (top-form (forms:get-top-form forms pos))
           (form (forms:get-outer-form top-form pos))
           (start (when form (form:xyz-get-start form)))
           (end (when form (form:get-end form))))
        (create-response id start end)))
