(defpackage :alive/session/handler/eval
    (:use :cl)
    (:export :handle)
    (:local-nicknames (:deps :alive/session/deps)
                      (:eval :alive/eval)
                      (:notification :alive/lsp/message/notification)
                      (:resp :alive/lsp/message/response)
                      (:state :alive/session/state)
                      (:utils :alive/session/utils)))

(in-package :alive/session/handler/eval)


(defun process-eval (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item 0))
           (** (state:get-history-item 1))
           (*** (state:get-history-item 2))
           (result (eval:from-string text
                                     :pkg-name pkg-name
                                     :stdin-fn (lambda ()
                                                   (utils:wait-for-input))
                                     :stdout-fn (lambda (data)
                                                    (deps:send-msg (notification:stdout data)))
                                     :trace-fn (lambda (data)
                                                   (deps:send-msg (notification:stdout data)))
                                     :stderr-fn (lambda (data)
                                                    (deps:send-msg (notification:stderr data))))))

        (when (cdr (assoc :store-result params))
              (state:add-history result))

        (deps:send-msg (resp:do-eval id
                                     (format nil "~A" result)))))


(defun handle (msg)
    (utils:run-in-thread (princ-to-string (cdr (assoc :id msg)))
                         msg
                         (lambda () (process-eval msg))))
