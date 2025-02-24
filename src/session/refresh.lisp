(defpackage :alive/session/refresh
    (:use :cl)
    (:export :send)
    (:local-nicknames (:deps :alive/deps)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session/refresh)


(declaim (ftype (function (deps:dependencies state:state) null) send))
(defun send (deps state)
    (spawn:new-thread "Refresh Thread"
        (let* ((send-id (state:next-send-id state))
               (response (deps:send-request deps (lsp-msg:create-request send-id "workspace/semanticTokens/refresh"))))
            (when (assoc :error response)
                  (logger:error-msg "Failed to refresh tokens: ~A" (cdr (assoc :error response))))
            (deps:send-msg deps (notification:refresh)))))
