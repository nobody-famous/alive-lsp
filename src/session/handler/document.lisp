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
             :references
             :selection
             :sem-tokens
             :sig-help)
    (:local-nicknames (:analysis :alive/lsp/sem-analysis)
                      (:comps :alive/lsp/completions)
                      (:config-item :alive/lsp/types/config-item)
                      (:fmt-opts :alive/lsp/types/format-options)
                      (:fmt-utils :alive/lsp/message/format-utils)
                      (:formatter :alive/format)
                      (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:range :alive/range)
                      (:selection :alive/selection)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:sig-help :alive/lsp/sig-help)
                      (:state :alive/session/state)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/document)


(declaim (ftype (function (state:state cons) hash-table) completion))
(defun completion (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (items (or (comps:simple :text text :pos pos)
                      (make-array 0))))

        (let ((data (make-hash-table :test #'equalp)))

            (setf (gethash "isIncomplete" data) T)
            (setf (gethash "items" data) items)

            (lsp-msg:create-response id :result-value data))))


(declaim (ftype (function (state:state cons) hash-table) definition))
(defun definition (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (location (alive/lsp/definition:get-location :text text :pos pos))
           (uri (first location))
           (range (second location)))

        (let ((data (make-hash-table :test #'equalp)))

            (setf (gethash "uri" data) uri)
            (setf (gethash "range" data) range)

            (lsp-msg:create-response id :result-value data))))


(declaim (ftype (function (state:state cons) null) did-change))
(defun did-change (state msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (changes (cdr (assoc :content-changes params)))
           (text (cdr (assoc :text (first changes)))))

        (when text
              (state:lock (state mutex)
                  (state:set-file-text state uri text)
                  nil))))


(declaim (ftype (function (state:state cons) null) did-open))
(defun did-open (state msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (cdr (assoc :text doc))))

        (when text
              (state:lock (state mutex)
                  (state:set-file-text state uri text)
                  nil))))


(declaim (ftype (function (state:state cons) hash-table) doc-symbols))
(defun doc-symbols (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (symbols (alive/lsp/symbol:for-document text forms)))

        (let ((result (or symbols (make-hash-table))))
            (lsp-msg:create-response id
                                     :result-value result))))


(declaim (ftype (function (state:state cons) hash-table) hover))
(defun hover (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (hov-text (alive/lsp/hover:get-text :text text :pos pos))
           (result (if hov-text hov-text "")))

        (utils:result id "value" result)))


(declaim (ftype (function (state:state cons) hash-table) on-type))
(defun on-type (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (opts (cdr (assoc :options params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (edits (formatter:on-type (make-string-input-stream text)
                                     :options (fmt-opts:convert opts)
                                     :pos pos))
           (value (if edits
                      (fmt-utils:to-text-edits edits)
                      (make-array 0))))

        (lsp-msg:create-response id :result-value value)))


(declaim (ftype (function (state:state cons cons) hash-table) format-msg))
(defun format-msg (state options msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (range (cdr (assoc :range params)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (edits (formatter:range (make-string-input-stream text)
                                   range
                                   options)))

        (lsp-msg:create-response id
                                 :result-value (fmt-utils:to-text-edits edits))))


(declaim (ftype (function (state:state cons) hash-table) formatting))
(defun formatting (state msg)
    (let ((id (state:next-send-id state)))

        (state:set-sent-msg-callback state id
                                     (lambda (config-resp)
                                         (declare (type cons config-resp))
                                         (let ((opts (cdr (assoc :result config-resp))))
                                             (format-msg state (first opts) msg))))

        (let ((params (make-hash-table :test #'equalp)))
            (setf (gethash "items" params) (list (config-item:create-item :section "alive.format")))
            (lsp-msg:create-request id "workspace/configuration" :params params))))


(declaim (ftype (function (state:state cons) hash-table) selection))
(defun selection (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (pos-list (cdr (assoc :positions params)))
           (ranges (when (and forms pos-list)
                         (selection:ranges forms pos-list))))

        (lsp-msg:create-response id :result-value (or ranges
                                                      (make-hash-table :test #'equalp)))))


(declaim (ftype (function (cons) cons) to-sem-array))
(defun to-sem-array (sem-tokens)
    (loop :with line := 0
          :with col := 0
          :with out-list := nil

          :for token :in sem-tokens
          :for len := (- (sem-types:end-col token) (sem-types:start-col token))
          :for line-diff := (- (sem-types:line token) line)
          :for col-diff := (if (zerop line-diff)
                               (- (sem-types:start-col token) col)
                               (sem-types:start-col token)) :do

              (push line-diff out-list)
              (push col-diff out-list)
              (push len out-list)
              (push (sem-types:token-type token) out-list)
              (push 0 out-list)

              (setf line (sem-types:line token))
              (setf col (sem-types:start-col token))
          :finally (return (reverse out-list))))


(declaim (ftype (function (state:state cons) hash-table) sem-tokens))
(defun sem-tokens (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (sem-tokens (analysis:to-sem-tokens
                           (tokenizer:from-stream
                               (make-string-input-stream text)))))

        (utils:result id "data" (if sem-tokens
                                    (to-sem-array sem-tokens)
                                    nil))))


(declaim (ftype (function (state:state cons) hash-table) sig-help))
(defun sig-help (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (pos (cdr (assoc :position params)))
           (text (or (state:get-file-text state uri) ""))
           (items (or (sig-help:signatures :text text :pos pos)
                      (make-array 0))))

        (utils:result id "signatures" items)))


(declaim (ftype (function (state:state cons) hash-table) references))
(defun references (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (pos (cdr (assoc :position params)))
           (text (or (state:get-file-text state uri) ""))
           (locs (alive/sys/xref:get-locations text pos)))
        (lsp-msg:create-response id
                                 :result-value (or locs (make-array 0)))))
