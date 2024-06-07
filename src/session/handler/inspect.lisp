(defpackage :alive/session/handler/inspect
    (:use :cl)
    (:export :do-close
             :do-inspect
             :do-symbol
             :macro
             :refresh)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:eval :alive/eval)
                      (:inspector :alive/inspector)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:macros :alive/macros)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/session/handler/inspect)


(defun inspect-response (id &key insp-id result result-type)
    (let ((data (make-hash-table :test #'equalp))
          (expr-type (if result-type result-type "expr")))

        (setf (gethash "id" data) insp-id)
        (setf (gethash "resultType" data) expr-type)
        (setf (gethash "result" data) result)

        (lsp-msg:create-response id :result-value data)))


(declaim (ftype (function (&key (:id integer) (:text string) (:pkg-name string) (:result *) (:result-type string) (:convert boolean)) null) send-inspect-result))
(defun send-inspect-result (&key id text pkg-name result (convert T) (result-type "expr"))
    (let ((insp-id (state:next-inspector-id)))
        (state:add-inspector insp-id
                             (inspector:create :text text
                                               :pkg pkg-name
                                               :result result))

        (deps:send-msg (inspect-response id
                                         :insp-id insp-id
                                         :result-type result-type
                                         :result (if convert
                                                     (inspector:to-result result)
                                                     (princ-to-string result))))))


(declaim (ftype (function (integer string string) null) try-inspect))
(defun try-inspect (id text pkg-name)
    (let ((result (eval:from-string text
                                    :pkg-name pkg-name
                                    :stdin-fn (lambda ()
                                                  (threads:wait-for-input))
                                    :stdout-fn (lambda (data)
                                                   (deps:send-msg (notification:stdout data)))
                                    :stderr-fn (lambda (data)
                                                   (deps:send-msg (notification:stderr data))))))

        (send-inspect-result :id id
                             :text text
                             :pkg-name pkg-name
                             :result result)))


(declaim (ftype (function (list) null) do-inspect))
(defun do-inspect (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (or (cdr (assoc :package params))
                         "cl-user"))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item 0))
           (** (state:get-history-item 1))
           (*** (state:get-history-item 2)))
        (handler-case
                (progn (unless (stringp text)
                           (error "No text to inspect"))
                       (try-inspect id text pkg-name))
            (T (c)
               (deps:send-msg (lsp-msg:create-error id
                                                    :code errors:*internal-error*
                                                    :message (princ-to-string c)))))))


(declaim (ftype (function (list) null) do-inspect-eval))
(defun do-inspect-eval (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params)))
           (text (cdr (assoc :text params)))
           (inspector (when insp-id (state:get-inspector insp-id)))
           (old-result (inspector:get-result inspector))
           (* (if (symbolp old-result)
                  (symbol-value old-result)
                  old-result))
           (pkg-name (inspector:get-pkg inspector))
           (new-result (eval:from-string text
                                         :pkg-name pkg-name
                                         :stdin-fn (lambda ()
                                                       (threads:wait-for-input))
                                         :stdout-fn (lambda (data)
                                                        (deps:send-msg (notification:stdout data)))
                                         :stderr-fn (lambda (data)
                                                        (deps:send-msg (notification:stderr data))))))

        (if new-result
            (send-inspect-result :id id
                                 :text text
                                 :pkg-name pkg-name
                                 :result new-result)

            (deps:send-msg (lsp-msg:create-response id
                                                    :result-value (make-hash-table))))))


(defun refresh (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params)))
           (inspector (when insp-id (state:get-inspector insp-id)))
           (result (inspector:get-result inspector)))

        (typecase result
            (symbol (deps:send-msg (inspect-response id
                                                     :insp-id insp-id
                                                     :result (inspector:to-result (if (fboundp result)
                                                                                      result
                                                                                      (symbol-value result))))))
            (otherwise (deps:send-msg (inspect-response id
                                                        :insp-id insp-id
                                                        :result (inspector:to-result result)))))))


(defun do-close (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params))))

        (state:rem-inspector insp-id)
        (lsp-msg:create-response id :result-value T)))


(defun do-symbol (msg)
    (let ((id (cdr (assoc :id msg))))

        (handler-case
                (let* ((params (cdr (assoc :params msg)))
                       (pkg-name (cdr (assoc :package params)))
                       (name (cdr (assoc :symbol params)))
                       (sym (alive/symbols:lookup name pkg-name)))

                    (send-inspect-result :id id
                                         :text name
                                         :pkg-name pkg-name
                                         :result sym))

            (T (c)
               (deps:send-msg (lsp-msg:create-error id
                                                    :code errors:*internal-error*
                                                    :message (princ-to-string c)))))))


(defun macro (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (expanded (macros:expand-1 text pkg-name)))

        (send-inspect-result :id id
                             :text text
                             :pkg-name pkg-name
                             :result-type "macro"
                             :convert NIL
                             :result expanded)))
