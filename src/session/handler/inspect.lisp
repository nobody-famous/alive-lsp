(defpackage :alive/session/handler/inspect
    (:use :cl)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:eval :alive/eval)
                      (:inspector :alive/inspector)
                      (:lsp-msg :alive/lsp/message/abstract)
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


(defun process-inspect (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item 0))
           (** (state:get-history-item 1))
           (*** (state:get-history-item 2)))
        (handler-case
                (try-inspect id text pkg-name)
            (T (c)
               (deps:send-msg (lsp-msg:create-error id
                                                    :code errors:*internal-error*
                                                    :message (princ-to-string c)))))))
