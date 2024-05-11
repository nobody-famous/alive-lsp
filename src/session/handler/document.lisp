(defpackage :alive/session/handler/document
    (:use :cl)
    (:export :completion
             :definition
             :did-change
             :did-open
             :doc-symbols
             :hover)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:forms :alive/parse/forms)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/document)


(declaim (ftype (function (cons) hash-table) completion))
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


(declaim (ftype (function (cons) hash-table) definition))
(defun definition (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (location (alive/lsp/definition:get-location :text text :pos pos))
           (uri (first location))
           (range (second location)))

        (let ((data (make-hash-table :test #'equalp)))

            (setf (gethash "uri" data) uri)
            (setf (gethash "range" data) range)

            (lsp-msg:create-response id :result-value data))))


(declaim (ftype (function (cons) null) did-change))
(defun did-change (msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (changes (cdr (assoc :content-changes params)))
           (text (cdr (assoc :text (first changes)))))

        (when text
              (state:lock
                  (state:set-file-text uri text)
                  nil))))


(declaim (ftype (function (cons) null) did-open))
(defun did-open (msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (cdr (assoc :text doc))))

        (when text
              (state:lock
                  (state:set-file-text uri text)
                  nil))))


(declaim (ftype (function (cons) hash-table) doc-symbols))
(defun doc-symbols (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (symbols (alive/lsp/symbol:for-document text forms)))

        (let ((result (if symbols symbols (make-hash-table))))
            (lsp-msg:create-response id
                                     :result-value result))))


(declaim (ftype (function (cons) hash-table) hover))
(defun hover (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (hov-text (alive/lsp/hover:get-text :text text :pos pos))
           (result (if hov-text hov-text "")))

        (utils:result id "value" result)))
