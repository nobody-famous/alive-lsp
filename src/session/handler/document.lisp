(defpackage :alive/session/handler/document
    (:use :cl)
    (:export :completion
             :definition
             :did-change
             :did-open
             :doc-symbols
             :formatting
             :hover
             :on-type
             :selection)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:config-item :alive/lsp/types/config-item)
                      (:fmt-opts :alive/lsp/types/format-options)
                      (:fmt-utils :alive/lsp/message/format-utils)
                      (:formatter :alive/format)
                      (:forms :alive/parse/forms)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:selection :alive/selection)
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


(declaim (ftype (function (cons) hash-table) on-type))
(defun on-type (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (opts (cdr (assoc :options params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (edits (formatter:on-type (make-string-input-stream text)
                                     :options (fmt-opts:convert opts)
                                     :pos pos))
           (value (if edits
                      (fmt-utils:to-text-edits edits)
                      (make-array 0))))

        (lsp-msg:create-response id :result-value value)))


(declaim (ftype (function (cons cons) hash-table) format-msg))
(defun format-msg (options msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (range (cdr (assoc :range params)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (edits (formatter:range (make-string-input-stream text)
                                   range
                                   options)))

        (lsp-msg:create-response id
                                 :result-value (fmt-utils:to-text-edits edits))))


(declaim (ftype (function (cons) hash-table) formatting))
(defun formatting (msg)
    (let ((id (state:next-send-id)))

        (state:set-sent-msg-callback id
                                     (lambda (config-resp)
                                         (declare (type cons config-resp))
                                         (let ((opts (cdr (assoc :result config-resp))))
                                             (format-msg (first opts) msg))))

        (let ((params (make-hash-table :test #'equalp)))
            (setf (gethash "items" params) (list (config-item:create-item :section "alive.format")))
            (lsp-msg:create-request id "workspace/configuration" :params params))))


(declaim (ftype (function (cons) hash-table) selection))
(defun selection (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (state:get-file-text uri))
           (text (if file-text file-text ""))
           (forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (pos-list (cdr (assoc :positions params)))
           (ranges (when (and forms pos-list)
                         (selection:ranges forms pos-list))))

        (lsp-msg:create-response id :result-value (or ranges
                                                      (make-hash-table :test #'equalp)))))
