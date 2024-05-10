(defpackage :alive/session/handler/document
    (:use :cl)
    (:export :completion)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/handler/document)


(defun completion (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (items (or (comps:simple :text text :pos pos)
                      (make-array 0))))

        (let ((data (make-hash-table :test #'equalp)))

            (setf (gethash "isIncomplete" data) T)
            (setf (gethash "items" data) items)

            (lsp-msg:create-response id :result-value data))))
