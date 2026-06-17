(defpackage :alive/session/handler/form-bounds
    (:use :cl)
    (:export :surrounding-form
             :top-form)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/form-bounds)


(defun get-forms (state msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) "")))

        (forms:xyz-from-stream-or-nil (make-string-input-stream text))))


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
           (forms (get-forms state msg))
           (form (forms:xyz-get-top-form forms pos))
           (start (when form (form:xyz-get-start form)))
           (end (when form (form:xyz-get-end form))))
        (create-response id start end)))


(defun surrounding-form (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pos (cdr (assoc :position params)))
           (forms (get-forms state msg))
           (top-form (forms:xyz-get-top-form forms pos))
           (form (forms:xyz-get-outer-form top-form pos))
           (start (when form (form:xyz-get-start form)))
           (end (when form (form:xyz-get-end form))))
        (create-response id start end)))
